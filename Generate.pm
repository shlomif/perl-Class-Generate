package Class::Generate;

require 5.004;
use strict;
use Carp;
use English;

BEGIN {
    use vars qw(@ISA @EXPORT_OK $VERSION);
    use vars qw($save $accept_refs $strict $allow_redefine $class_var $instance_var $check_code $check_default $nfi);

    require Exporter;
    @ISA = qw(Exporter);
    @EXPORT_OK = (qw(&class &subclass), qw($save $accept_refs $strict $allow_redefine $class_var $instance_var $check_code $check_default $nfi));
    $VERSION = '1.03';

    $accept_refs    = 1;
    $strict	    = 1;
    $allow_redefine = 0;
    $class_var	    = 'class';
    $instance_var   = 'self';
    $check_code	    = 1;
    $check_default  = 1;
    $nfi	    = 0;
}

use vars qw(@_initial_values);	# Holds all initial values passed as references.

my ($class_name, $class);
my ($class_vars, $use_packages, $excluded_methods, $param_style_spec, $default_pss);
my %class_options;
my %classes;			# A record of all classes defined.
my %base;			# The base types of all classes.

my $cm;				# These variables are for error messages.
my $sa_needed		 = 'must be string or array reference';
my $sh_needed		 = 'must be string or hash reference';
my $parent_class_unknown = 'Parent class "%s" was not defined using class() or subclass()';

my $allow_redefine_for_class;

my ($initialize,				# These are all used to
    $parse_any_flags,				# hold package-local
    $set_class_type,				# subs that other
    $parse_class_specification,			# packages shouldn't call.
    $parse_method_specification,
    $parse_member_specification,
    $set_attributes,
    $class_defined,
    $process_class,
    $store_initial_value_reference,
    $set_positional_parameters_order,
    $constructor_parameter_passing_style,
    $croak_if_duplicate_names,
    $invalid_spec_message);

my %valid_option  = map((substr($_,1) => 1), grep substr($_,0,1) eq '$', @EXPORT_OK);

sub class(%) {					# One of the two interface
    my %params = @_;				# routines to the package.
    &$initialize();				# Defines a class.
    &$parse_any_flags(\%params);
    croak "Missing/extra arguments to class()"		if scalar(keys %params) != 1;
    ($class_name, undef) = %params;
    $cm = qq|Class "$class_name"|;
    croak "$cm: A package of this name already exists"	if ! $allow_redefine_for_class && &$class_defined($class_name);
    &$set_class_type($params{$class_name});
    &$process_class($params{$class_name});
}

sub subclass(%) {				# One of the two interface
    my %params = @_;				# routines to the package.
    &$initialize();				# Defines a subclass.
    my ($p_spec, $parent);
    if ( defined ($p_spec = $params{-parent}) ) {
	delete $params{-parent};
    }
    elsif ( defined ($p_spec = $params{parent}) ) {
	delete $params{parent};
	carp qq|Use of "parent" is deprecated (use "-parent" instead)| if $WARNING;
    }
    else {
	croak "Missing subclass parent";
    }
    eval { $parent = Class::Generate::Array->new($p_spec) };
    croak qq|Invalid parent specification ($sa_needed)|		if $@ || scalar($parent->values) == 0;
    &$parse_any_flags(\%params);
    croak "Missing/extra arguments to subclass()"		if scalar(keys %params) != 1;
    ($class_name, undef) = %params;
    $cm = qq|Subclass "$class_name"|;
    croak "$cm: A package of this name already exists"		if ! $allow_redefine_for_class && &$class_defined($class_name);
    for my $p ( $parent->values ) {
	croak qq|$cm: Parent package "$p" does not exist|	if ! &$class_defined($p);
	croak qq|$cm: Base type does not match that of "$p"|	if exists $classes{$p} &&
								   ! UNIVERSAL::isa($params{$class_name}, $base{$p});
    }
    &$set_class_type($params{$class_name}, $parent);
    for my $p ( $parent->values ) {
	if ( defined $classes{$p} ) {
	    $class->add_parents($classes{$p});
	}
	else {
	    carp sprintf("$cm: $parent_class_unknown", $p)	if $WARNING;
	    $class->add_parents($p);
	}
    }
    &$process_class($params{$class_name});
}

$default_pss = Class::Generate::Array->new('key_value');

$initialize = sub {			# Reset certain variables, and set
    undef $class_vars;			# options to their default values.
    undef $use_packages;
    undef $excluded_methods;
    $param_style_spec = $default_pss;
    %class_options = ( virtual	    => 0,
		       strict	    => $strict,
		       save	    => $save,
		       accept_refs  => $accept_refs,
		       class_var    => $class_var,
		       instance_var => $instance_var,
		       check_code   => $check_code,
		       check_default=> $check_default,
		       nfi	    => $nfi );
    $allow_redefine_for_class = $allow_redefine;
};

$set_class_type = sub {			# Set $class to the type (array or
    my ($class_spec, $parent) = @_;	# hash) appropriate to its declaration.
    my @params = ($class_name, %class_options);
    if ( UNIVERSAL::isa($class_spec, 'ARRAY') ) {
	if ( defined $parent ) {
	    my ($parent_name, @rest) = $parent->values;
	    croak qq|$cm: An array reference based subclass must have exactly one parent|
		if @rest;
	    croak sprintf("$cm: $parent_class_unknown", $parent_name)
		if ! exists $classes{$parent_name};
	    push @params, ( base_index => $classes{$parent_name}->last + 1 );
	}
	$class = Class::Generate::Array_Class->new(@params);
    }
    elsif ( UNIVERSAL::isa($class_spec, 'HASH') ) {
	$class = Class::Generate::Hash_Class->new(@params);
    }
    else {
	croak qq|$cm: Elements must be in array or hash reference|;
    }
};

my $class_name_regexp	 = '[A-Za-z_]\w*(::[A-Za-z_]\w*)*';

$parse_class_specification = sub {	# Parse the class' specification,
    my %specs = @_;			# checking for errors and amalgamating
    my %required;			# class data.

    if ( defined $specs{new} ) {
	croak qq|$cm: Specification for "new" must be hash reference|
	    unless UNIVERSAL::isa($specs{new}, 'HASH');
	my %new_spec = %{$specs{new}};	# Modify %new_spec, not parameter passed
	my $required_items;		# to class() or subclass().
	if ( defined $new_spec{required} ) {
	    eval { $required_items = Class::Generate::Array->new($new_spec{required}) };
	    croak qq|$cm: Invalid specification for required constructor parameters ($sa_needed)| if $@;
	    delete $new_spec{required};
	}
	if ( defined $new_spec{style} ) {
	    eval { $param_style_spec = Class::Generate::Array->new($new_spec{style}) };
	    croak qq|$cm: Invalid parameter-passing style ($sa_needed)| if $@;
	    delete $new_spec{style};
	}
	$class->constructor(Class::Generate::Constructor->new(%new_spec));
	if ( defined $required_items ) {
	    for ( $required_items->values ) {
		if ( /^\w+$/ ) {
		    croak qq|$cm: Required params list for constructor contains unknown member "$_"|
			if ! defined $specs{$_};
		    $required{$_} = 1;
		}
		else {
		    $class->constructor->add_constraints($_);
		}
	    }
	}
    }
    else {
	$class->constructor(Class::Generate::Constructor->new);
    }

    my $actual_name;
    for my $member_name ( grep $_ ne 'new', keys %specs ) {
	$actual_name = $member_name;
	$actual_name =~ s/^&//;
	croak qq|$cm: Invalid member/method name "$actual_name"| unless $actual_name =~ /^[A-Za-z_]\w*$/;
	croak qq|$cm: "$instance_var" is reserved|		 unless $actual_name ne $class_options{instance_var};
	if ( substr($member_name, 0, 1) eq '&' ) {
	    &$parse_method_specification($member_name, $actual_name, \%specs);
	}
	else {
	    &$parse_member_specification($member_name, \%specs, \%required);
	}
    }
    $class->constructor->style(&$constructor_parameter_passing_style);
};

$parse_method_specification = sub {
    my ($member_name, $actual_name, $specs) = @_;
    my (%spec, $method);
    my $invalid_spec_error = &$invalid_spec_message('method', $actual_name, 'body');

    croak $invalid_spec_error if ! defined $$specs{$member_name};
    eval { %spec = %{Class::Generate::Hash->new($$specs{$member_name}, 'body')} };
    croak $invalid_spec_error if $@;
    $method = Class::Generate::Method->new($actual_name, $spec{body});
    if ( $spec{class_method} ) {
	bless $method, 'Class::Generate::Class_Method';
	if ( $spec{objects} ) {
	    my $objects;
	    eval { $objects = Class::Generate::Array->new($spec{objects}) };
	    croak qq|$cm: Invalid specification for objects of "$actual_name" ($sa_needed)| if $@;
	    $method->add_objects($objects->values);
	}
	delete $spec{objects} if exists $spec{objects};
    }
    delete $spec{class_method} if exists $spec{class_method};
    $class->user_defined_methods($actual_name, $method);
    &$set_attributes($actual_name, $method, 'Method', 'body', \%spec);
};

$parse_member_specification = sub {
    my ($member_name, $specs, $required) = @_;
    my (%spec, $member, %member_params);
    my $invalid_spec_error = &$invalid_spec_message('member', $member_name, 'type');

    croak $invalid_spec_error if ! defined $$specs{$member_name};
    eval { %spec = %{Class::Generate::Hash->new($$specs{$member_name}, 'type')} };
    croak $invalid_spec_error if $@;

    $spec{required} = 1 if $$required{$member_name};
    if ( exists $spec{default} ) {
	if ( $WARNING && $class_options{check_default} ) {
	    eval { Class::Generate::Support::verify_value($spec{default}, $spec{type}) };
	    carp qq|$cm: Default value for "$member_name" is not correctly typed| if $@;
	}
	&$store_initial_value_reference(\$spec{default}, $member_name) if ref $spec{default};
	$member_params{default} = $spec{default};
    }
    %member_params = map defined $spec{$_} ? ($_ => $spec{$_}) : (), qw(post pre assert);
    if ( $spec{type} =~ m/[\$@%]?($class_name_regexp)/o ) {
	$member_params{base} = $1;
    }
    elsif ( $spec{type} !~ m/[\$\@\%]/ ) {
	croak qq|$cm: Member "$member_name": "$spec{type}" is not a valid type|;
    }
    if ( $spec{required} && $spec{private} ) {
	carp qq|$cm: "required" attribute ignored for private member "$member_name"| if $WARNING;
	delete $spec{required};
    }
    delete @member_params{grep ! defined $member_params{$_}, keys %member_params};
    if ( substr($spec{type}, 0, 1) eq '@' ) {
	$member = Class::Generate::Array_Member->new($member_name, %member_params);
    }
    elsif ( substr($spec{type}, 0, 1) eq '%' ) {
	$member = Class::Generate::Hash_Member->new($member_name, %member_params);
    }
    else {
	$member = Class::Generate::Scalar_Member->new($member_name, %member_params);
    }
    delete $spec{type};
    $class->members($member_name, $member);
    &$set_attributes($member_name, $member, 'Member', undef, \%spec);
};

$parse_any_flags = sub {
    my $params = $_[0];
    my %flags = map(($_, $$params{$_}), grep(substr($_, 0, 1) eq '-', keys %$params));
    return if ! %flags;
  flag:
    while ( my ($flag, $value) = each %flags ) {
	$flag eq '-use' and do {
	    eval { $use_packages = Class::Generate::Array->new($value) };
	    croak qq|"-use" flag $sa_needed| if $@;
	    next flag;
	};
	$flag eq '-class_vars' and do {
	    eval { $class_vars = Class::Generate::Array->new($value) };
	    croak qq|"-class_vars" flag $sa_needed| if $@;
	    for my $var_spec ( $class_vars->values ) {
		next if ! ref $var_spec;
		croak qq|Each class variable must be scalar or hash reference|
		    unless UNIVERSAL::isa($var_spec, 'HASH');
		for my $var ( keys %$var_spec ) {
		    &$store_initial_value_reference(\$$var_spec{$var}, $var) if ref $$var_spec{$var};
		}
	    }
	    next flag;
	};
	$flag eq '-virtual' and do {
	    $class_options{virtual} = $value;
	    next flag;
	};
	$flag eq '-exclude' and do {
	    eval { $excluded_methods = Class::Generate::Array->new($value) };
	    croak qq|"-exclude" flag $sa_needed| if $@;
	    next flag;
	};
	$flag eq '-comment' and do {
	    $class_options{comment} = $value;
	    next flag;
	};
	$flag eq '-options' and do {
	    croak qq|Options must be in hash reference| unless UNIVERSAL::isa($value, 'HASH');
	    if ( exists $$value{allow_redefine} ) {
		$allow_redefine_for_class = $$value{allow_redefine};
		delete $$value{allow_redefine};
	    }
	  option:
	    while ( my ($o, $o_value) = each %$value ) {
		if ( ! $valid_option{$o} ) {
		     carp qq|Unknown option "$o" ignored| if $WARNING;
		     next option;
		 }
		$class_options{$o} = $o_value;
	    }
	    next flag;
	};
	carp qq|Unknown flag "$flag" ignored| if $WARNING;
    }
    delete @$params{keys %flags};
};
				# Set the appropriate attributes of
$set_attributes = sub {		# a member or method w.r.t. a class.
    my ($name, $m, $type, $exclusion, $spec) = @_;
    for my $attr ( defined $exclusion ? grep($_ ne $exclusion, keys %$spec) : keys %$spec ) {
	if ( $m->can($attr) ) {
	    $m->$attr($$spec{$attr});
	}
	elsif ( $class->can($attr) ) {
	    $class->$attr($name, $$spec{$attr});
	}
	else {
	    carp qq|$cm: $type "$name": Unknown attribute "$attr"| if $WARNING;
	}
    }
};

my $containing_package = __PACKAGE__ . '::';
my $initial_value_form = $containing_package . '_initial_values';

$store_initial_value_reference = sub {		# Store initial values that are
    my ($default_value, $var_name) = @_;	# references in an accessible
    push @_initial_values, $$default_value;	# place.
    $$default_value = "\$$initial_value_form" . "[$#_initial_values]";
    carp qq|Cannot save reference as initial value for "$var_name"|
	if $class_options{save} && $WARNING;
};

$class_defined = sub {			# Return TRUE if the argument
    my $class_name = $_[0];		# is the name of a Perl package.
    return eval 'defined %'  . $class_name . '::';
};
					# Do the main work of processing a class.
$process_class = sub {			# Parse its specification, generate a
    my $class_spec = $_[0];		# form, and evaluate that form.
    my (@warnings, $errors);
    &$croak_if_duplicate_names($class_spec);
    for my $var ( grep defined $class_options{$_}, qw(instance_var class_var) ) {
	croak qq|$cm: Value of $var option must be an identifier (without a "\$")|
	    unless $class_options{$var} =~ /^[A-Za-z_]\w*$/;
    }
    &$parse_class_specification(UNIVERSAL::isa($class_spec, 'ARRAY') ? @$class_spec : %$class_spec);
    Class::Generate::Member_Names::set_element_regexps();
    $class->add_class_vars($class_vars->values)		    if $class_vars;
    $class->add_use_packages($use_packages->values)	    if $use_packages;
    $class->excluded_methods_regexp(join '|', map "(?:$_)", $excluded_methods->values)
							    if $excluded_methods;
    if ( $WARNING && $class_options{check_code} ) {
	Class::Generate::Code_Checker::check_user_defined_code($class, $cm, \@warnings, \$errors);
	for my $warning ( @warnings ) {
	    carp $warning;
	}
	carp $errors if $errors;
    }

    my $form = $class->form;
    if ( $class_options{save} ) {
	my ($class_file, $ob, $cb);
	if ( $class_options{save} =~ /\.p[ml]$/ ) {
	    $class_file = $class_options{save};
	    open CLASS_FILE, ">>$class_file" or croak qq|$cm: Cannot append to "$class_file": $!|;
	    $ob = "{\n";	# The form is enclosed in braces to prevent
	    $cb = "}\n";	# renaming duplicate "my" variables.
	}
	else {
	    $class_file = $class_name . '.pm';
	    $class_file =~ s|::|/|g;
	    open CLASS_FILE, ">$class_file" or croak qq|$cm: Cannot save to "$class_file": $!|;
	    $ob = $cb = '';
	}
	$form =~ s/^(my [%@\$]\w+) = ([%@]\{)?\$$initial_value_form\[\d+\]\}?;/$1;/mgo;
	print CLASS_FILE $ob, $form, $cb, "\n1;\n";
	close CLASS_FILE;
    }
    croak "$cm: Cannot continue after errors" if $errors;
    {
	local $WARNING = undef;		# Warnings have been reported during
	eval $form;			# user-defined code analysis.
	if ( $@ ) {
	    croak "$cm: Evaluation failed (missing final semicolon in pre/post code?)";
	}
    }
    $classes{$class_name} = $class;
    $base{$class_name} = (UNIVERSAL::isa($class_spec, 'ARRAY') ? 'ARRAY' : 'HASH');
};

$constructor_parameter_passing_style = sub {	# Establish the parameter-passing style
    my ($style,					# for a class' constructor, meanwhile
        @values,				# checking for mismatches w.r.t. the
	$parent_with_constructor,		# class' superclass. Return an
	$parent_constructor_package_name);	# appropriate style.
    if ( defined $class->parents ) {
	$parent_with_constructor = Class::Generate::Support::class_containing_method('new', $class);
	$parent_constructor_package_name = (ref $parent_with_constructor ? $parent_with_constructor->name : $parent_with_constructor);
    }
    (($style, @values) = $param_style_spec->values)[0] eq 'key_value' and do {
	if ( defined $parent_with_constructor && ref $parent_with_constructor && index(ref $parent_with_constructor, $containing_package) == 0 ) {
	    my $invoked_constructor_style = $parent_with_constructor->constructor->style;
	    unless ( $invoked_constructor_style->isa($containing_package . 'Key_Value') ||
		     $invoked_constructor_style->isa($containing_package . 'Own') ) {
		carp $cm, ': Probable mismatch calling constructor in superclass "', $parent_constructor_package_name, '"' if $WARNING;
	    }
	}
	return Class::Generate::Key_Value->new('params');
    };
    $style eq 'positional' and do {
	my $s = Class::Generate::Positional->new;
	my @member_names = $class->nonprivate_member_names;
	croak "$cm: Missing/extra members in style" unless $#values == $#member_names;
	&$set_positional_parameters_order($s, @values);
	return $s;
    };
    $style eq 'mix' and do {
	my $s = Class::Generate::Mix->new('params');
	my @member_names = $class->members_keys;
	croak "$cm: Extra parameters in style specifier" unless $#values <= $#member_names;
	&$set_positional_parameters_order($s, @values);
	return $s;
    };
    $style eq 'own' and do {
	for ( my $i = 0; $i <= $#values; $i++ ) {
	    &$store_initial_value_reference(\$values[$i], $parent_constructor_package_name . '::new') if ref $values[$i];
	}
	return Class::Generate::Own->new([@values]);
    };
    croak qq|$cm: Invalid parameter passing style "$style"|;
};

$set_positional_parameters_order = sub {	# For parameter-passing styles with
    my ($style, @param_names) = @_;		# positional orderings, set the order
    my $i = 0;					# in which the parameters are passed.
    for my $param ( @param_names ) {
	croak qq|$cm: Error in new => { style => '... $param' }: $param is not a member|
	    unless defined $class->members($param);
	$style->order($param, $i++);
    }
};

$croak_if_duplicate_names = sub {
    my $class_spec = $_[0];
    my (@names, %uses);
    if ( UNIVERSAL::isa($class_spec, 'ARRAY') ) {
	for ( my $i = 0; $i <= $#$class_spec; $i += 2 ) {
	    push @names, $$class_spec[$i];
	}
    }
    else {
	@names = keys %$class_spec;
    }
    for ( @names ) {
	$uses{substr($_, 0, 1) eq '&' ? substr($_, 1) : $_}++;
    }
    %uses = map(($uses{$_} > 1 ? ($_ => $uses{$_}) : ()), keys %uses);
    if ( %uses ) {
	croak "$cm: ", join('; ', map qq|Name "$_" used $uses{$_} times|, keys %uses);
    }
};

$invalid_spec_message = sub {
    return sprintf qq|$cm: Invalid specification of %s "%s" ($sh_needed with "%s" key)|, @_;
};

package Class::Generate::Member_Names;	# This package encapsulates functions
use strict;				# to handle name substitution in
					# user-defined code.

my ($method_regexp,		# Regexp of member methods.
    $accessor_regexp,		# Regexp of member and other accessors.
    $private_member_regexp);	# (For class methods) Regexp of accessors
				# for private members.

sub set_element_regexps() {		# Establish the regexps for
    my @names;				# name substitution.

	# First for members...
    @names = accessible_member_regexps($class);
    if ( ! @names ) {
	undef $method_regexp;
    }
    else {
	$method_regexp = '(' . join('|', sort { length $b <=> length $a } @names) . ')\b';
    }

	# Next for accessors (e.g., x_size) and instance methods...
    @names = (map($_->accessor_names($class), $class->members_values),
	      grep $class->include_method($_), map($_->name, $class->instance_methods));
    if ( ! @names ) {
	undef $accessor_regexp;
    }
    else {
	$accessor_regexp = '&(' . join('|', sort { length $b <=> length $a } @names) . ')\b';
    }

	# Finally for private members and instance methods in class methods.
    if ( $class->class_methods ) {
	@names = (map($_->accessor_names($class), grep $class->private($_->name), $class->members_values),
		  grep($class->private($_), map($_->name, $class->instance_methods)));
	if ( ! @names ) {
	    undef $private_member_regexp;
	}
	else {
	    $private_member_regexp = join('|', sort { length $b <=> length $a } @names);
	}
    }
    else {
	undef $private_member_regexp;
    }
}

sub substituted($) {			# Within a code fragment, replace
    my $code = $_[0];			# member names and accessors with the
					# appropriate forms.
    $code =~ s/$method_regexp/method_invocation($1)/eg     if defined $method_regexp;
    $code =~ s/$accessor_regexp/accessor_invocation($1)/eg if defined $accessor_regexp;
    return $code;
}

sub method_invocation($) {		# Perform the actual substitution
    my $member_reference = $_[0];	# for element references.
    my ($name, $type, $form, $index);
    $member_reference =~ /^(\W+)(\w+)$/;
    $name = $2;
    return $member_reference if ! defined ($index = member_index($class, $name));
    $type = $1;
    $form = $class->instance_var . '->' . $index;
    return $form if $type eq '$';
    return $type . '{' . $form . '}';
}

sub accessor_invocation($) {		# Perform the actual substitution
    my $name = $_[0];			# for accessor references.
    my $prefix = $class->instance_var . '->';
    return $prefix . $name		if ! $class->private($name);
    return $prefix . '$' . $name	if $' =~ /\A\s*\(/;
    return $prefix . '$' . $name . '()';
}

sub substituted_in_class_method {
    my $method = $_[0];
    my (@objs, $code, $private_member_invocation_regexp);
    $code = $method->body;
    return $code if ! (defined $private_member_regexp && (@objs = $method->objects));
    $private_member_invocation_regexp = '(' . join('|', map(quotemeta($_), @objs)) . ')' .
					'\s*->\s*(' . $private_member_regexp . ')' .
					'(\s*\((?:\s*\))?)?';
    $code =~ s/$private_member_invocation_regexp/private_method_invocation($1, $2, $3)/ge;
    return $code;
}

sub private_method_invocation {				# Perform the actual
    my ($object, $private_member, $paren_matter) = @_;	# substitution for
    my $form = '&$' . $private_member . '(' . $object;	# private method and
    if ( defined $paren_matter ) {			# member references.
	if ( index($paren_matter, ')') != -1 ) {
	    $form .= ')';
	}
	else {
	    $form .= ', ';
	}
    }
    else {
	$form .= ')';
    }
    return $form;
}

sub member_index($$) {
    my ($class, $member_name) = @_;
    return $class->index($member_name) if defined $class->members($member_name);
    for my $parent ( grep ref $_, $class->parents ) {
	my $index = member_index($parent, $member_name);
	return $index if defined $index;
    }
    return undef;
}

sub accessible_member_regexps($;$) {
    my ($class, $disallow_private_members) = @_;
    my @members;
    if ( $disallow_private_members ) {
	@members = grep ! $class->private($_->name), $class->members_values;
    }
    else {
	@members = $class->members_values;
    }
    return (map($_->method_regexp($class), @members),
	    map(accessible_member_regexps($_, 1), grep(ref $_, $class->parents)));
}

sub accessible_members($;$) {
    my ($class, $disallow_private_members) = @_;
    my @members;
    if ( $disallow_private_members ) {
	@members = grep ! $class->private($_->name), $class->members_values;
    }
    else {
	@members = $class->members_values;
    }
    return (@members, map(accessible_members($_, 1), grep(ref $_, $class->parents)));
}

package Class::Generate::Code_Checker;		# This package encapsulates
use strict;					# checking for warnings and
use English;					# errors in user-defined code.
use Carp;

my $package_decl;
my $member_error_message = '%s, member "%s": In "%s" code: %s';
my $method_error_message = '%s, method "%s": %s';

sub create_code_checking_package($);
sub fragment_as_sub($$\@;\@);
sub collect_code_problems($$$$@);

# Check each user-defined code fragment in $class for errors. This includes
# pre, post, and assert code, as well as user-defined methods.  Set
# $errors_found according to whether errors (not warnings) were found.
sub check_user_defined_code($$$$) {
    my ($class, $class_name_label, $warnings, $errors) = @_;
    my ($code, $instance_var, @valid_variables, @class_vars, $w, $e, @members, $problems_in_pre, %seen);
    create_code_checking_package $class;
    @valid_variables = map { $seen{$_->name} ? () : do { $seen{$_->name} = 1; $_->as_var } }
			     ((@members = $class->members_values),
			    Class::Generate::Member_Names::accessible_members($class));
    @class_vars = $class->class_vars;
    $instance_var = $class->instance_var;
    @$warnings = ();
    undef $$errors;
    for my $member ( $class->constructor, @members ) {
	if ( defined ($code = $member->pre) ) {
	    $code = fragment_as_sub $code, $instance_var, @class_vars, @valid_variables;
	    collect_code_problems $code,
				  $warnings, $errors,
				  $member_error_message, $class_name_label, $member->name, 'pre';
	    $problems_in_pre = @$warnings || $$errors;
	}
	# Because post shares pre's scope, check post with pre prepended.
	# Strip newlines in pre to preserve line numbers in post.
	if ( defined ($code = $member->post) ) {
	    my $pre = $member->pre;
	    if ( defined $pre && ! $problems_in_pre ) {	# Don't report errors
		$pre =~ s/\n+/ /g;			# in pre again.
		$code = $pre . $code;
	    }
	    $code = fragment_as_sub $code, $instance_var, @class_vars, @valid_variables;
	    collect_code_problems $code,
				  $warnings, $errors,
				  $member_error_message, $class_name_label, $member->name, 'post';
	}
	if ( defined ($code = $member->assert) ) {
	    $code = fragment_as_sub $code, $instance_var, @class_vars, @valid_variables;
	    collect_code_problems $code,
				  $warnings, $errors,
				  $member_error_message, $class_name_label, $member->name, 'assert';
	}
    }
    for my $method ( $class->user_defined_methods_values ) {
	if ( $method->isa('Class::Generate::Class_Method') ) {
	    $code = fragment_as_sub $method->body, $class->class_var, @class_vars;
	}
	else {
	    $code = fragment_as_sub $method->body, $instance_var, @class_vars, @valid_variables;
	}
	collect_code_problems $code, $warnings, $errors, $method_error_message, $class_name_label, $method->name;
    }
}

sub create_code_checking_package($) {	# Each class with user-defined code gets
    my $class = $_[0];			# its own package in which that code is
					# evaluated.  Create said package.
    $package_decl = 'package ' . __PACKAGE__ . '::check::' . $class->name . ";";
    $package_decl .= "use strict;" if $class->strict;
    my $packages = '';
    $packages .= 'use Carp;' if $WARNING;
    $packages .= join('', map('use ' . $_ . ';', $class->use_packages));
    eval $package_decl . $packages;
}
					# Evaluate a code fragment, passing on
sub collect_code_problems($$$$@) {	# warnings and errors.
    my ($code_form,  $warnings, $errors, $error_message, @params) = @_;
    my @warnings;
    local $SIG{__WARN__} = sub { push @warnings, $_[0] };
    local $SIG{__DIE__};
    eval $package_decl . $code_form;
    for my $w ( @warnings ) {
	push @$warnings, sprintf $error_message, @params, $w;
    }
    $$errors .= sprintf($error_message, @params, $@) if $@;
}

sub fragment_as_sub($$\@;\@) {
    my ($code, $id_var, $class_vars, $valid_vars) = @_;
    my $form;
    $form  = "sub{my $id_var;";
    if ( $#$class_vars >= 0 ) {
	$form .= 'my(' . join(',', map((ref $_ ? keys %$_ : $_), @$class_vars)) . ');';
    }
    if ( $valid_vars && $#$valid_vars >= 0 ) {
	$form .= 'my(' . join(',', @$valid_vars) . ');';
    }
    $form .= $code . '};';
}

package Class::Generate::Array;		# Given a string or an ARRAY, return an
use strict;				# object that is either the ARRAY or
use Carp;				# the string made into an ARRAY by
					# splitting the string on white space.
sub new {
    my $class = shift;
    my $self;
    if ( ! ref $_[0] ) {
	$self = [ split /\s+/, $_[0] ];
    }
    elsif ( UNIVERSAL::isa($_[0], 'ARRAY') ) {
	$self = $_[0];
    }
    else {
	croak 'Expected string or array reference';
    }
    bless $self, $class;
    return $self;
}

sub values {
    my $self = shift;
    return @$self;
}

package Class::Generate::Hash;		# Given a string or a HASH and a key
use strict;				# name, return an object that is either
use Carp;				# the HASH or a HASH of the form
					# (key => string). Also, if the object
sub new {				# is a HASH, it *must* contain the key.
    my $class = shift;
    my $self;
    my ($value, $key) = @_;
    if ( ! ref $value ) {
	$self = { $key => $value };
    }
    else {
	croak 'Expected string or hash reference' unless UNIVERSAL::isa($value, 'HASH');
	croak qq|Missing "$key"| unless exists $value->{$key};
	$self = $value;
    }
    bless $self, $class;
    return $self;
}

package Class::Generate::Support;	# Miscellaneous support routines.
no strict;				# Definitely NOT strict!
use English;
					# Return the superclass of $class that
sub class_containing_method($$) {	# contains the method that the form
    my ($method, $class) = @_;		# (new $class)->$method would invoke.
    for my $parent ( $class->parents ) {# Return undef if no such class exists.
	local *stab = eval ('*' . (ref $parent ? $parent->name : $parent) . '::');
	local *method_entry = $stab{$method};
	if ( defined *method_entry && defined &method_entry ) {
	    return $parent;
	}
	return class_containing_method($method, $parent);
    }
    return undef;
}

my %map = ('@' => 'ARRAY', '%' => 'HASH');
sub verify_value($$) {			# Die if a given value (ref or string)
    my ($value, $type) = @_;		# is not the specified type.
    if ( $type =~ /^\w/ ) {
	$map{$type} = $type;
    }
    else {
	$type = substr $type, 0, 1;
    }
    return if $type eq '$';
    local $WARNING = 0;
    my $result;
    $result = ref $value ? $value : eval $value;
    die "Wrong type" if ! UNIVERSAL::isa($result, $map{$type});
}

use strict;
sub comment_form {		# Given arbitrary text, return a form that
    my $comment = $_[0];	# is a valid Perl comment of that text.
    $comment =~ s/^/# /mg;
    $comment .= "\n" if substr($comment, -1, 1) ne "\n";
    return $comment;
}

package Class::Generate::Member;	# A virtual class describing class
use strict;				# members.
use English;

sub new {
    my $class = shift;
    my $self = { name => $_[0], @_[1..$#_] };
    bless $self, $class;
    return $self;
}
sub name {
    my $self = shift;
    return $self->{'name'};
}
sub default {
    my $self = shift;
    return $self->{'default'} if $#_ == -1;
    $self->{'default'} = $_[0];
}
sub base {
    my $self = shift;
    return $self->{'base'} if $#_ == -1;
    $self->{'base'} = $_[0];
}
sub assert {
    my $self = shift;
    return $self->{'assert'} if $#_ == -1;
    $self->{'assert'} = $_[0];
}
sub post {
    my $self = shift;
    return $self->{'post'} if $#_ == -1;
    $self->{'post'} = $_[0];
}
sub pre {
    my $self = shift;
    return $self->{'pre'} if $#_ == -1;
    $self->{'pre'} = $_[0];
}
sub comment {
    my $self = shift;
    return $self->{'comment'};
}
sub key {
    my $self = shift;
    return $self->{'key'} if $#_ == -1;
    $self->{'key'} = $_[0];
}
sub assertion {					# Return a form that croaks if
    my $self = shift;				# the member's assertion fails.
    my $class = $_[0];
    my $assertion = $self->{'assert'};
    return undef if ! defined $assertion;
    my $quoted_form = $assertion;
    $quoted_form =~ s/'/\\'/g;
    $assertion = Class::Generate::Member_Names::substituted($assertion);
    return qq|unless ( $assertion ) { croak '| . $self->name_form($class) . qq|Failed assertion: $quoted_form' }|;
}

sub param_message {		# Encapsulate the messages for
    my $self = shift;		# incorrect parameters.
    my $class = $_[0];
    my $name = $self->name;
    my $prefix_form = q|croak '| . $class->name . '::new' . ': ';
    $class->required($name) && ! $self->default and do {
	return $prefix_form . qq|Missing or invalid "$name" parameter'| if $self->can_be_invalid;
	return $prefix_form . qq|Missing "$name" parameter'|;
    };
    $self->can_be_invalid and do {
	return $prefix_form . qq|Invalid "$name" parameter'|;
    };
}

sub param_test {		# Return a form that dies if a constructor
    my $self = shift;		# parameter is not correctly passed.
    my $class  = $_[0];
    my $name	 = $self->name;
    my $param	 = $class->constructor->style->ref($name);
    my $exists	 = $class->constructor->style->existence_test($name) . ' ' . $param;

    my $form = '';
    if ( $class->required($name) && ! $self->default ) {
	$form .= $self->param_message($class) . ' unless ' . $exists;
	$form .= ' && ' . $self->valid_value_form($param) if $self->can_be_invalid;
    }
    elsif ( $self->can_be_invalid ) {
	$form .= $self->param_message($class) . ' unless ! ' . $exists . ' || ' . $self->valid_value_form($param);
    }
    return $form . ';';
}

sub form {				# Return a form for a member and all
    my $self = shift;			# its relevant associated accessors.
    my $class = $_[0];
    my ($element, $exists, $lvalue, $values, $form, $body);
    $element = $class->instance_var . '->' . $class->index($self->name);
    $exists  = $class->existence_test . ' ' . $element;
    $lvalue  = $self->lvalue('$_[0]')					if $self->can('lvalue');
    $values  = $self->values('$_[0]')					if $self->can('values');

    $form = '';
    $form .= Class::Generate::Support::comment_form($self->comment)	if defined $self->comment;

    if ( $class->include_method($self->name) ) {
	$body = '';
	for my $param_form ( $self->member_forms($class) ) {
	    $body .= $self->$param_form($class, $element, $exists, $lvalue, $values);
	}
	$body .= '    ' . $self->param_count_error_form($class) . ";\n" if $WARNING;
	$form .= $class->sub_form($self->name, $body);
    }
    $form .= $self->readonly_methods($class, $element, $self->name, $exists)
									if $self->can('readonly_methods');
    $form .= $self->modifying_methods($class, $element, $self->name)	if ! $class->readonly($self->name) && $self->can('modifying_methods');
    return $form;
}

sub invalid_value_assignment_message {	# Return a form that dies
    my $self = shift;			# unless a parameter is of the
    my $class = $_[0];			# correct type for its element.
    return q|croak '| . $self->name_form($class) . q|Invalid parameter value'|;
}
sub valid_value_test_form {		# Return a form that dies unless
    my $self = shift;			# a value is of the correct type
    my $class = shift;			# for the member.
    return $self->invalid_value_assignment_message($class) . ' unless ' . $self->valid_value_form(@_) . ';';
}
sub param_must_be_checked {
    my $self = shift;
    my $class = $_[0];
    return ($class->required($self->name) && ! defined $self->default) || $self->can_be_invalid;
}

sub maybe_guarded {			# If warnings are enabled, guard a
    my $self = shift;			# form to check against a parameter
    my ($form, $param_no) = @_;		# count.
    if ( $WARNING ) {
	$form .= "return;\n" if $form !~ /return.*\Z/;
	$form =~ s/^/\t/mg;
	return "    \$#_ == $param_no\tand do {\n${form}    };\n";
    }
    else {
	$form =~ s/^/    /mg;
	return $form;
    }
}

sub undef_form {			# Return the form to undefine
    my $self = shift;			# a member.
    my ($class, $element, $member_name) = @_;
    return '' if ! $class->include_method('undef_' . $member_name);
    return "sub undef_$member_name {\n" .
	   '    my ' . $class->instance_var . " = shift;\n" .
	   '    ' . $class->undef_form . " $element;\n" .
	   "}\n";
}

sub param_count_error_form {	# Return a form that standardizes
    my $self = shift;		# the message for dieing because
    my $class = $_[0];		# of an incorrect parameter count.
    return q|croak '| . $self->name_form($class) . q|Invalid number of parameters (', ($#_+1), ')'|;
}

sub name_form {			# Standardize a method name
    my $self = shift;		# for error messages.
    my $class = $_[0];
    return $class->name . '::' . $self->name . ': ';
}

sub param_assignment_form {	# Return a form that assigns a parameter
    my $self = shift;		# value to the member.
    my ($class, $style) = @_;
    my ($name, $element, $param, $default, $exists);
    $name     = $self->name;
    $element  = $class->instance_var . '->' . $class->index($name);
    $param    = $style->ref($name);
    $default  = $self->default;
    $exists   = $style->existence_test($name) . ' ' . $param;
    my $form = "    $element = ";
    $form .= defined $default ? "$exists ? $param : $default" : "$param if $exists";
    return $form . ";\n";
}

sub default_assignment_form {	# Return a form that assigns a default value
    my $self = shift;		# to a member.
    my $class = $_[0];
    my $element;
    $element  = $class->instance_var . '->' . $class->index($self->name);
    return "    $element = " . $self->default . ";\n";
}

package Class::Generate::Scalar_Member;		# A Member subclass for
use strict;					# scalar class members.
use English;					# Understands that the
use vars qw(@ISA);				# accessor accepts 0 or 1 parameters.
@ISA = qw(Class::Generate::Member);

sub member_forms {
    my $self = shift;
    my $class = $_[0];
    return $class->readonly($self->name) ? 'no_params' : ('no_params', 'one_param');
}
sub no_params {
    my $self = shift;
    my ($class, $element) = @_;
    if ( $class->readonly($self->name) && ! $WARNING ) {
	return "    return $element;\n";
    }
    return "    \$#_ == -1\tand do { return $element };\n";
}
sub one_param {
    my $self = shift;
    my ($class, $element) = @_;
    my $form = '';
    $form .= Class::Generate::Member_Names::substituted($self->pre)    if defined $self->pre;
    $form .= $self->valid_value_test_form($class, '$_[0]') . "\n"      if $WARNING && defined $self->base;
    $form .= "$element = \$_[0];\n";
    $form .= Class::Generate::Member_Names::substituted($self->post)   if defined $self->post;
    $form .= $self->assertion($class) . "\n"			       if $WARNING && defined $self->assert;
    return $self->maybe_guarded($form, 0);
}

sub valid_value_form {			# Return a form that tests if
    my $self = shift;			# a ref is of the correct
    my ($param) = @_;			# base type.
    return qq|UNIVERSAL::isa($param, '| . $self->base . qq|')|;
}

sub can_be_invalid {			# Validity for a scalar member
    my $self = shift;			# is testable only if the member
    return defined $self->base;		# is supposed to be a class.
}

sub as_var {
    my $self = shift;
    return '$' . $self->name;
}

sub method_regexp {
    my $self = shift;
    my $class = $_[0];
    return $class->include_method($self->name) ? ('\$' . $self->name) : ();
}
sub accessor_names {
    my $self = shift;
    my $class = $_[0];
    return grep $class->include_method($_), ($self->name, 'undef_' . $self->name);
}
sub modifying_methods {
    my $self = shift;
    my $class = $_[0];
    return $self->undef_form(@_) if ! $class->required($self->name);
    return '';
}

sub copy_form {
    my $self = shift;
    my ($from, $to) = @_;
    my $form = "    $to = $from";
    $form .= '->copy' if $self->base;
    $form .= " if defined $from;\n";
    return $form;
}

sub equals {
    my $self = shift;
    my ($index, $existence_test) = @_;
    my ($sr, $or) = ('$self->' . $index, '$o->' . $index);
    my $form = "    return undef if $existence_test $sr ^ $existence_test $or;\n" .
	       "    if ( $existence_test $sr ) { return undef unless $sr"; 
    if ( $self->base ) {
	$form .= "->equals($or)";
    }
    else {
	$form .= " eq $or";
    }
    return $form . " }\n";
}

package Class::Generate::List_Member;		# A Member subclass for list
use strict;					# (array and hash) members.
use English;					# Understands that the
use vars qw(@ISA);				# accessor accepts 0-2 parameters.
@ISA = qw(Class::Generate::Member);

sub member_forms {
    my $self = shift;
    my $class = $_[0];
    return $class->readonly($self->name) ? ('no_params', 'one_param') : ('no_params', 'one_param', 'two_params');
}
sub no_params {
    my $self = shift;
    my ($class, $element, $exists, $lvalue, $values) = @_;
    return "    \$#_ == -1\tand do { return $exists ? " . $self->whole_lvalue($element) . " : () };\n";
}
sub one_param {
    my $self = shift;
    my ($class, $element, $exists, $lvalue, $values) = @_;
    my $form;
    if ( $class->accept_refs ) {
	$form  = "    \$#_ == 0\tand do {\n" .
		 "\t" . "return ($exists ? ${element}->$lvalue : undef)	if ! ref \$_[0];\n";
	if ( $WARNING && $class->readonly($self->name) ) {
	    $form .= "croak '" . $self->name_form($class) . "Member is read-only';\n";
	}
	else {
	    $form .= "\t" . Class::Generate::Member_Names::substituted($self->pre)  if defined $self->pre;
	    $form .= "\t" . $self->valid_value_test_form($class, '$_[0]')  . "\n"   if $WARNING;
	    $form .= "\t" . $self->whole_lvalue($element) . ' = ' . $self->whole_lvalue('$_[0]') . ";\n";
	    $form .= "\t" . Class::Generate::Member_Names::substituted($self->post) if defined $self->post;
	    $form .= "\t" . $self->assertion($class) . "\n"			    if $WARNING && defined $self->assert;
	    $form .= "\t" . "return;\n";
	}
	$form .= "    };\n";
    }
    else {
	$form  = "    \$#_ == 0\tand do { return $exists ? ${element}->$lvalue : undef };\n"
    }
    return $form;
}
sub two_params {
    my $self = shift;
    my ($class, $element, $exists, $lvalue, $values) = @_;
    my $form = '';
    $form .= Class::Generate::Member_Names::substituted($self->pre)		if defined $self->pre;
    $form .= $self->valid_element_test($class, '$_[1]') . "\n"			if $WARNING && defined $self->base;
    $form .= "${element}->$lvalue = \$_[1];\n";
    $form .= Class::Generate::Member_Names::substituted($self->post)		if defined $self->post;
    return $self->maybe_guarded($form, 1);
}

sub valid_value_form {			# Return a form that tests if a
    my $self = shift;			# parameter is a correct list reference
    my $param = $_[0];			# and (if relevant) if all of its
    my $base = $self->base;		# elements have the correct base type.
    my $form = 'UNIVERSAL::isa(' . $param . ", '" . $self->ref_isa($param) . "')";
    $form .= (qq| && ! grep ! UNIVERSAL::isa(\$_, '$base'), | . $self->values($param)) if defined $base;
    return $form;
}

sub valid_element_test {		# Return a form that dies unless an
    my $self = shift;			# element has the correct base type.
    my ($class, $param) = @_;
    my $base = $self->base;
    return $self->invalid_value_assignment_message($class) . ' unless ' .
	   qq|UNIVERSAL::isa($param, '$base');|;
}

sub valid_elements_test {		# Return a form that dies unless all
    my $self = shift;			# elements of a list are validly typed.
    my ($class, $values) = @_;
    my $base = $self->base;
    return $self->invalid_value_assignment_message($class) . ' unless ' .
	   qq|! grep ! UNIVERSAL::isa(\$_, '$base'), $values;|;
}

sub can_be_invalid {		# A value for a list member can
    return 1;			# always be invalid: the wrong
}				# type of list can be given.

package Class::Generate::Array_Member;		# A List subclass for array
use strict;					# members.  Provides the
use English;					# n_size method, plus specifics
use vars qw(@ISA);				# of accessing array members.
@ISA = qw(Class::Generate::List_Member);

sub lvalue {
    my $self = shift;
    return '[' . $_[0] . ']';
}

sub whole_lvalue {
    my $self = shift;
    return '@{' . $_[0] . '}';
}

sub values {
    my $self = shift;
    return '@{' . $_[0] . '}';
}

sub ref_isa {
    return 'ARRAY';
}

sub modifying_methods {
    my $self = shift;
    my ($class, $element, $member_name) = @_;
    my $form = '';

    if ( $class->include_method('add_' . $member_name) ) {
	$form .=  "sub add_$member_name {\n" .
	    '    my ' . $class->instance_var . " = shift;\n";
	$form .=  '    ' . $self->valid_elements_test($class, '@_') . "\n"	    if defined $self->base && $WARNING;
	$form .=	   Class::Generate::Member_Names::substituted($self->pre)   if defined $self->pre;
	$form .=  '    push @{' . $element . '}, @_;' . "\n";
	$form .=	   Class::Generate::Member_Names::substituted($self->post)  if defined $self->post;
	$form .=  '    ' . $self->assertion($class) . "\n"			    if $WARNING && defined $self->assert;
	$form .=  "}\n";
    }
    $form .=  $self->undef_form($class, $element, $member_name)		    if ! $class->required($member_name);
    return $form;
}

sub readonly_methods {
    my $self = shift;
    my ($class, $element, $member_name, $exists) = @_;
    my $form = '';
    $form .= ('sub ' . $member_name . "_size {\n" .
	      '    my ' . $class->instance_var . " = shift;\n" .
	      "    return $exists ? \$#{$element} : -1;\n" .
	      "}\n")
	if $class->include_method($member_name . '_size');
    $form .= ("sub last_$member_name {\n" .
	      '    my ' . $class->instance_var . " = shift;\n" .
	      "    return $exists ? $element" . "[\$#{$element}] : undef;\n" .
	      "}\n")
	if $class->include_method('last_' . $member_name);
    return $form;
}

sub as_var {
    my $self = shift;
    return '@' . $self->name;
}

sub method_regexp {
    my $self = shift;
    my $class = $_[0];
    return $class->include_method($self->name) ? ('@' . $self->name, '\$#?' . $self->name) : ();
}
sub accessor_names {
    my $self = shift;
    my $class = $_[0];
    my $name = $self->name;
    return grep $class->include_method($_), ($name, 'add_' . $name, $name . '_size', 'undef_' . $name, 'last_' . $name);
}

sub copy_form {
    my $self = shift;
    my ($from, $to) = @_;
    my $form = "    $to = [ ";
    if ( $self->base ) {
	$form .= "map defined \$_ ? \$_->copy : undef, ";
    }
    $form .= "\@{$from} ] if defined $from;\n";
    return $form;
}

sub equals {
    my $self = shift;
    my ($index, $existence_test) = @_;
    my ($sr, $or) = ('$self->' . $index, '$o->' . $index);
    my $form = "    return undef if $existence_test($sr) ^ $existence_test($or);\n" .
	       "    if ( $existence_test $sr ) {\n" .
	       "	return undef unless (\$ub = \$#{$sr}) == \$#{$or};\n" .
	       "	for ( my \$i = 0; \$i <= \$ub; \$i++ ) {\n" .
	       "	    return undef unless $sr" . '[$i]';
    if ( $self->base ) {
	$form .= '->equals(' . $or . '[$i])';
    }
    else {
	$form .= ' eq ' . $or . '[$i]';
    }
    return $form . ";\n\t}\n    }\n";
}

package Class::Generate::Hash_Member;		# A List subclass for Hash
use strict;					# members.  Provides the n_keys
use English;					# and n_values, methods, plus
use vars qw(@ISA);				# specifics of accessing
@ISA = qw(Class::Generate::List_Member);	# hash members.

sub lvalue {
    my $self = shift;
    return '{' . $_[0] . '}';
}
sub whole_lvalue {
    my $self = shift;
    return '%{' . $_[0] . '}';
}
sub values {
    my $self = shift;
    return 'values %{' . $_[0] . '}';
}

sub ref_isa {
    return 'HASH';
}

sub modifying_methods {
    my $self = shift;
    my $class = $_[0];
    return $self->undef_form(@_) if ! $class->required($self->name);
}

sub readonly_methods {
    my $self = shift;
    my ($class, $element, $member_name, $exists) = @_;
    my $form = '';
    $form .= ("sub ${member_name}_keys {\n" .
	      '    my ' . $class->instance_var . " = shift;\n" .
	      "    return $exists ? keys \%{$element} : ();\n" .
	      "}\n")
	if $class->include_method($member_name . '_keys');
    $form .= ("sub ${member_name}_values {\n" .
	      '    my ' . $class->instance_var . " = shift;\n" .
	      "    return $exists ? values \%{$element} : ();\n" .
	    "}\n")
	if $class->include_method($member_name . '_values');
    return $form;
}

sub as_var {
    my $self = shift;
    return '%' . $self->name;
}

sub method_regexp {
    my $self = shift;
    my $class = $_[0];
    return $class->include_method($self->name) ? ('[%$]' . $self->name) : ();
}
sub accessor_names {
    my $self = shift;
    my $class = $_[0];
    my $name = $self->name;
    return grep $class->include_method($_), ($name, $name . '_keys', $name . '_values', 'undef_' . $name);
}

sub copy_form {
    my $self = shift;
    my ($from, $to) = @_;
    my $form;
    if ( $self->base ) {
	$form = "    if ( defined $from ) {\n" .
	        "\t$to = {};\n" .
	        "\twhile ( my (\$key, \$value) = each \%{$from} ) {\n" .
		"\t    $to" . '->{$key} = defined $value ? $value->copy : undef;' . "\n" .
		"\t}\n" .
		"    }\n";
    }
    else {
	$form = "    $to = { \%{$from} } if defined $from;\n";
    }
    return $form;
}

sub equals {
    my $self = shift;
    my ($index, $existence_test) = @_;
    my ($sr, $or) = ('$self->' . $index, '$o->' . $index);
    my $form = "    return undef if $existence_test $sr ^ $existence_test $or;\n" .
	       "    if ( $existence_test $sr ) {\n" .
	       '	@self_keys = keys %{' . $sr . '};' . "\n" .
	       '	return undef unless $#self_keys == scalar(keys %{' . $or . '}) - 1;' . "\n" .
	       '	for my $k ( @self_keys ) {' . "\n" .
	       "	    return undef unless exists $or" . '{$k};' . "\n" .
	       '	    return undef if ($self_value_defined = defined ' . $sr . '{$k}) ^ defined ' . $or . '{$k};' . "\n" .
	       '	    if ( $self_value_defined ) { return undef unless ';
    if ( $self->base ) {
	$form .= $sr . '{$k}->equals(' . $or . '{$k})';
    }
    else {
	$form .= $sr . '{$k} eq ' . $or . '{$k}';
    }
    $form .= " }\n\t}\n    }\n";
    return $form;
}

package Class::Generate::Constructor;	# The constructor is treated as a
use strict;				# special type of member.  It includes
use English;				# constraints on required members.
use vars qw(@ISA);
@ISA = qw(Class::Generate::Member);

sub new {
    my $class = shift;
    my $self = $class->SUPER::new('new', @_);
    return $self;
}
sub style {
    my $self = shift;
    return $self->{'style'} if $#_ == -1;
    $self->{'style'} = $_[0];
}
sub constraints {
    my $self = shift;
    return exists $self->{'constraints'} ? @{$self->{'constraints'}} : () if $#_ == -1;
    return exists $self->{'constraints'} ? $self->{'constraints'}->[$_[0]] : undef if $#_ == 0;
    $self->{'constraints'}->[$_[0]] = $_[1];
}
sub add_constraints {
    my $self = shift;
    push @{$self->{'constraints'}}, @_;
}
sub constraints_size {
    my $self = shift;
    return exists $self->{'constraints'} ? $#{$self->{'constraints'}} : -1;
}
sub form {
    my $self = shift;
    my $class = $_[0];
    my $style = $self->style;
    my ($iv, $cv) = ($class->instance_var, $class->class_var);
    my $form;
    $form  = "sub new {\n" .
	     "    my $cv = " .
		 ($class->nfi ? 'do { my $proto = shift; ref $proto || $proto }' : 'shift') .
		 ";\n";
    if ( $class->virtual && $WARNING ) {
	$form .= q|    croak '| . $self->name_form($class) . q|Virtual class' unless $class ne '| . $class->name . qq|';\n|;
    }
    $form .= $style->init_form($class, $self)		if ! $class->can_assign_all_params &&
							   $style->can('init_form'); 
    if ( $WARNING && ! $class->parents && $style->can('params_check_form') ) {
	$form .= $style->params_check_form($class, $self);
    }
    if ( $WARNING && ! $style->isa('Class::Generate::Own') ) {
	my @param_tests = map $_->param_test($class), grep $_->param_must_be_checked($class), $class->members_values;
	if ( @param_tests ) {
	    $form .= '    ' . join("\n    ", @param_tests) . "\n";
	}
	for my $constraint ( $self->constraints ) {
	    my $param_given = $constraint;
	    $param_given =~ s/\w+/$style->existence_test($&) . ' ' . $style->ref($&)/eg;
	    $constraint =~ s/'/\\'/g;
	    $form .= q|    croak '| . $self->name_form($class) . qq|Parameter constraint "$constraint" failed' unless $param_given;\n|;
	}
    }
    if ( defined $class->parents ) {
	$form .=  $style->self_from_super_form($class);
    }
    else {
	$form .= '    my ' . $iv . ' = ' . $class->base . ";\n" .
		 '    bless ' . $iv . ', ' . $cv . ";\n";
    }
    if ( ! $class->can_assign_all_params ) {
	$form .= $class->size_establishment($iv)    if $class->can('size_establishment');
	if ( ! $style->isa('Class::Generate::Own') ) {
	    for my $name ( $class->nonprivate_member_names ) {
		$form .= $class->members($name)->param_assignment_form($class, $style);
	    }
	}
    }
    for my $member ( grep(($style->isa('Class::Generate::Own') || $class->private($_->name)) &&
			  defined $_->default, $class->members_values) ) {
	$form .= $member->default_assignment_form($class);
    }
    $form .= Class::Generate::Member_Names::substituted($self->post) if defined $self->post;
    if ( $WARNING ) {
	$form .= '    ' . $self->assertion($class) . "\n"	     if defined $self->assert;
	for my $member ( grep defined $_->assert, $class->members_values ) {
	    $form .= '    ' . $member->assertion($class) . "\n";
	}
    }
    $form .= '    return ' . $iv . ";\n" .
	     "}\n";
    return $form;
}

package Class::Generate::Method;	# A user-defined method,
					# with a name and body.
sub new {
    my $class = shift;
    my $self = { name => $_[0], body => $_[1] };
    bless $self, $class;
    return $self;
}

sub name {
    my $self = shift;
    return $self->{'name'};
}

sub body {
    my $self = shift;
    return $self->{'body'};
}

sub comment {
    my $self = shift;
    return return $self->{'comment'} if $#_ == -1;
    $self->{'comment'} = $_[0];
}

sub form {
    my $self = shift;
    my $class = $_[0];
    my $form = '';
    $form .= Class::Generate::Support::comment_form($self->comment) if defined $self->comment;
    $form .= $class->sub_form($self->name, Class::Generate::Member_Names::substituted($self->body));
    return $form;
}

package Class::Generate::Class_Method;	# A user-defined class method,
use strict;				# which may specify objects
use vars qw(@ISA);			# of the class used within its
@ISA = qw(Class::Generate::Method);	# body.

sub objects {
    my $self = shift;
    return exists $self->{'objects'} ? @{$self->{'objects'}} : ()	   if $#_ == -1;
    return exists $self->{'objects'} ? $self->{'objects'}->[$_[0]] : undef if $#_ == 0;
    $self->{'objects'}->[$_[0]] = $_[1];
}
sub add_objects {
    my $self = shift;
    push @{$self->{'objects'}}, @_;
}

sub form {
    my $self = shift;
    my $class = $_[0];
    my $form = '';
    $form .= Class::Generate::Support::comment_form($self->comment) if defined $self->comment;
    $form .= 'sub ' . $self->name . " {\n" .
	     '    my ' . $class->class_var . " = shift;\n" .
	     Class::Generate::Member_Names::substituted_in_class_method($self) .
	     "}\n";
    return $form;
}

package Class::Generate::Class;			# A virtual class describing
use strict;					# a user-specified class.
use English;

sub new {
    my $class = shift;
    my $self = { name => shift, @_ };
    bless $self, $class;
    return $self;
}

sub name {
    my $self = shift;
    return $self->{'name'};
}
sub parents {
    my $self = shift;
    return exists $self->{'parents'} ? @{$self->{'parents'}} : ()	   if $#_ == -1;
    return exists $self->{'parents'} ? $self->{'parents'}->[$_[0]] : undef if $#_ == 0;
    $self->{'parents'}->[$_[0]] = $_[1];
}
sub add_parents {
    my $self = shift;
    push @{$self->{'parents'}}, @_;
}
sub members {
    my $self = shift;
    return exists $self->{'members'} ? %{$self->{'members'}} : ()	   if $#_ == -1;
    return exists $self->{'members'} ? $self->{'members'}->{$_[0]} : undef if $#_ == 0;
    $self->{'members'}->{$_[0]} = $_[1];
}
sub members_keys {
    my $self = shift;
    return exists $self->{'members'} ? keys %{$self->{'members'}} : ();
}
sub members_values {
    my $self = shift;
    return exists $self->{'members'} ? values %{$self->{'members'}} : ();
}
sub user_defined_methods {
    my $self = shift;
    return exists $self->{'udm'} ? %{$self->{'udm'}} : ()	   if $#_ == -1;
    return exists $self->{'udm'} ? $self->{'udm'}->{$_[0]} : undef if $#_ == 0;
    $self->{'udm'}->{$_[0]} = $_[1];
}
sub user_defined_methods_keys {
    my $self = shift;
    return exists $self->{'udm'} ? keys %{$self->{'udm'}} : ();
}
sub user_defined_methods_values {
    my $self = shift;
    return exists $self->{'udm'} ? values %{$self->{'udm'}} : ();
}
sub class_vars {
    my $self = shift;
    return exists $self->{'class_vars'} ? @{$self->{'class_vars'}} : ()		 if $#_ == -1;
    return exists $self->{'class_vars'} ? $self->{'class_vars'}->[$_[0]] : undef if $#_ == 0;
    $self->{'class_vars'}->[$_[0]] = $_[1];
}
sub add_class_vars {
    my $self = shift;
    push @{$self->{'class_vars'}}, @_;
}
sub use_packages {
    my $self = shift;
    return exists $self->{'use_packages'} ? @{$self->{'use_packages'}} : ()	     if $#_ == -1;
    return exists $self->{'use_packages'} ? $self->{'use_packages'}->[$_[0]] : undef if $#_ == 0;
    $self->{'use_packages'}->[$_[0]] = $_[1];
}
sub add_use_packages {
    my $self = shift;
    push @{$self->{'use_packages'}}, @_;
}
sub excluded_methods_regexp {
    my $self = shift;
    return $self->{'em'} if $#_ == -1;
    $self->{'em'} = $_[0];
}
sub private {
    my $self = shift;
    return exists $self->{'private'} ? %{$self->{'private'}} : ()	   if $#_ == -1;
    return exists $self->{'private'} ? $self->{'private'}->{$_[0]} : undef if $#_ == 0;
    $self->{'private'}->{$_[0]} = $_[1];
}
sub required {
    my $self = shift;
    return exists $self->{'required'} ? %{$self->{'required'}} : ()	     if $#_ == -1;
    return exists $self->{'required'} ? $self->{'required'}->{$_[0]} : undef if $#_ == 0;
    $self->{'required'}->{$_[0]} = $_[1];
}
sub readonly {
    my $self = shift;
    return exists $self->{'readonly'} ? %{$self->{'readonly'}} : ()	     if $#_ == -1;
    return exists $self->{'readonly'} ? $self->{'readonly'}->{$_[0]} : undef if $#_ == 0;
    $self->{'readonly'}->{$_[0]} = $_[1];
}
sub constructor {
    my $self = shift;
    return $self->{'constructor'} if $#_ == -1;
    $self->{'constructor'} = $_[0];
}
sub virtual {
    my $self = shift;
    return $self->{'virtual'} if $#_ == -1;
    $self->{'virtual'} = $_[0];
}
sub comment {
    my $self = shift;
    return $self->{'comment'} if $#_ == -1;
    $self->{'comment'} = $_[0];
}
sub accept_refs {
    my $self = shift;
    return $self->{'accept_refs'};
}
sub strict {
    my $self = shift;
    return $self->{'strict'};
}
sub nfi {
    my $self = shift;
    return $self->{'nfi'};
}
sub instance_methods {
    my $self = shift;
    return grep ! $_->isa('Class::Generate::Class_Method'), $self->user_defined_methods_values;
}
sub class_methods {
    my $self = shift;
    return grep $_->isa('Class::Generate::Class_Method'), $self->user_defined_methods_values;
}
sub include_method {
    my $self = shift;
    my $method_name = $_[0];
    my $r = $self->excluded_methods_regexp;
    return ! defined $r || $method_name !~ m/$r/;
}
sub member_methods_form {	# Return a form containing methods for all
    my $self = shift;		# non-private members in the class, plus
    my $form = '';		# private members used in class methods.
    for my $element ( $self->nonprivate_member_names, $self->private_members_used_in_class_methods ) {
	$form .= $self->members($element)->form($self);
    }
    $form .= "\n" if $form ne '';
    return $form;
}

sub user_defined_methods_form {		# Return a form containing all
    my $self = shift;			# user-defined methods.
    my $form = join('', map($_->form($self), $self->user_defined_methods_values));
    return length $form > 0 ? $form . "\n" : '';
}

sub form {				# Return a form representing
    my $self = shift;			# a class.
    my $form;
    $form  = 'package ' . $self->name . ";\n";
    $form .= "use strict;\n"						     if $self->strict;
    $form .= join("\n", map("use $_;", $self->use_packages)) . "\n"	     if defined $self->use_packages;
    $form .= "use Carp;\n" if $WARNING;
    $form .= "\n";
    $form .= Class::Generate::Support::comment_form($self->comment)	     if defined $self->comment;
    $form .= $self->isa_decl_form					     if defined $self->parents;
    $form .= $self->private_methods_decl_form				     if grep $self->private($_), $self->user_defined_methods_keys;
    $form .= $self->private_members_decl_form				     if defined $self->private_members_used_in_class_methods;
    $form .= join("\n", map(class_var_form($_), $self->class_vars)) . "\n\n" if defined $self->class_vars;
    $form .= $self->constructor->form($self)				     if $self->needs_constructor;
    $form .= $self->member_methods_form;
    $form .= $self->user_defined_methods_form;
    my $emr = $self->excluded_methods_regexp;
    $form .= $self->copy_form		if ! defined $emr || 'copy' !~ m/$emr/;
    $form .= $self->equals_form		if (! defined $emr || 'equals' !~ m/$emr/) &&
					   ! defined $self->user_defined_methods('equals');
    return $form;
}

sub class_var_form {			# Return a form for declaring a class
    my $var_spec = $_[0];		# variable.  Account for an initial value.
    return "my $var_spec;" if ! ref $var_spec;
    return map { my $value = $$var_spec{$_}; 
		 "my $_ = " . (ref $value ? substr($_, 0, 1) . "{$value}" : $value) . ';'
		 } keys %$var_spec;
}

sub isa_decl_form {
    my $self = shift;
    my @parent_names = map ! ref $_ ? $_ : $_->name, $self->parents;
    return "use vars qw(\@ISA);\n" .
	   '@ISA = qw(' . join(' ', @parent_names) . ");\n";
}

sub sub_form {
    my $self = shift;
    my ($name, $body) = @_;
    my ($form, $is_private);
    $form = (($is_private = $self->private($name)) ? "\$$name = sub" : "sub $name") . " {\n" .
            '    my ' . $self->instance_var . " = shift;\n" .
	    $body .
	    '}';
    $form .= ';' if $is_private;
    return $form . "\n";
}

sub private_methods_decl_form {		# Private methods are implemented as CODE refs.
    my $self = shift;			# Return a form declaring the variables to hold them.
    my @private_methods = grep $self->private($_), $self->user_defined_methods_keys;
    $#private_methods == 0 and do {
	return 'my $' . $private_methods[0] . ";\n";
    };
    $#private_methods > 0  and do {
	return 'my (' . join(', ', map('$' . $_, @private_methods)) . ");\n";
    };
}

sub private_members_used_in_class_methods {	# Return the names of all private
    my $self = shift;				# members called by class methods.
    my @private_members = grep $self->private($_), $self->members_keys;
    return () if ! @private_members;
    my $member_regexp = join '|', @private_members;
    my %private_members;
    for my $method ( $self->class_methods ) {
	my $body = $method->body;
	while ( $body =~ /($member_regexp)/g ) {
	    $private_members{$1}++;
	}
    }
    return keys %private_members;
}

sub private_members_decl_form {
    my $self = shift;
    my @private_members = $self->private_members_used_in_class_methods;
    $#private_members == 0 and do {
	return 'my $' . $private_members[0] . ";\n";
    };
    $#private_members > 0 and do {
	return 'my (' . join(', ', map('$' . $_, @private_members)) . ");\n";
    };
}

sub copy_form {
    my $self = shift;
    my ($form, @members, $has_parents);
    @members = $self->members_values;
    $has_parents = defined $self->parents;
    $form = "sub copy {\n" .
	    "    my \$self = shift;\n" .
	    "    my \$copy;\n";
    if ( ! (do { my $has_complex_mems;
		 for my $m ( @members ) {
		     if ( $m->isa('Class::Generate::List_Member') || defined $m->base ) {
			 $has_complex_mems = 1;
			 last;
		     }
		 }
		 $has_complex_mems
	    } || $has_parents) ) {
	$form .= '    $copy = ' . $self->wholesale_copy . ";\n";
    }
    else {
	$form .= '    $copy = ' . ($has_parents ? '$self->SUPER::copy' : $self->empty_form) . ";\n";
	$form .= $self->size_establishment('$copy')	if $self->can('size_establishment');
	for my $m ( @members ) {
	    my $index = $self->index($m->name);
	    $form .= $m->copy_form('$self->' . $index, '$copy->' . $index);
	}
    }
    $form .= "    bless \$copy, ref \$self;\n" .
	     "    return \$copy;\n" .
	     "}\n";
    return $form;
}

sub equals_form {
    my $self = shift;
    my ($form, @parents, @members, $existence_test, @local_vars, @key_members);
    @parents = $self->parents;
    @members = $self->members_values;
    if ( @key_members = grep $_->key, @members ) {
	@members = @key_members;
    }
    $existence_test = $self->existence_test;
    $form = "sub equals {\n" .
	    "    my \$self = shift;\n" .
	    "    my \$o = \$_[0];\n";
    for my $m ( @members ) {
	if ( $m->isa('Class::Generate::Hash_Member'), @members ) {
	    push @local_vars, qw($self_value_defined @self_keys);
	    last;
	}
    }
    for my $m ( @members ) {
	if ( $m->isa('Class::Generate::Array_Member'), @members ) {
	    push @local_vars, qw($ub);
	    last;
	}
    }
    if ( @local_vars ) {
	$form .= '    my (' . join(', ', @local_vars) . ");\n";
    }
    if ( @parents ) {
	$form .= "    return undef unless \$self->SUPER::equals(\$o);\n";
    }
    $form .= join("\n", map $_->equals($self->index($_->name), $existence_test), @members) .
	"    return 1;\n" .
	"}\n";
    return $form;
}

sub all_members_required {
    my $self = shift;
    for my $m ( $self->members_keys ) {
	return 0 if ! ($self->private($m) || $self->required($m));
    }
    return 1;
}
sub nonprivate_member_names {
    my $self = shift;
    return grep ! $self->private($_), $self->members_keys;
}

sub class_var {
    my $self = shift;
    return '$' . $self->{'class_var'};
}
sub instance_var {
    my $self = shift;
    return '$' . $self->{'instance_var'};
}
sub needs_constructor {
    my $self = shift;
    return (defined $self->members ||
	    ($self->virtual && $WARNING) ||
	    scalar($self->parents) == 0 ||
	    do {
		my $c = $self->constructor;
		(defined $c->post ||
		 defined $c->assert ||
		 $c->style->isa('Class::Generate::Own'))
		});
}

package Class::Generate::Array_Class;		# A subclass of Class defining
use strict;					# array-based classes.
use English;
use vars qw(@ISA);
@ISA = qw(Class::Generate::Class);

sub new {
    my $class = shift;
    my $name = shift;
    my %params = @_;
    my %super_params = %params;
    delete @super_params{qw(base_index member_index)};
    my $self = $class->SUPER::new($name, %super_params);
    $self->{'base_index'} = defined $params{'base_index'} ? $params{'base_index'} : 0;
    $self->{'next_index'} = $self->base_index - 1;
    return $self;
}

sub base_index {
    my $self = shift;
    return $self->{'base_index'};
}
sub base {
    my $self = shift;
    return '[]' if ! $self->can_assign_all_params;
    my @sorted_members = sort { $$self{member_index}{$a} <=> $$self{member_index}{$b} } $self->members_keys;
    my %param_indices  = map(($_, $self->constructor->style->order($_)), $self->members_keys);
    for ( my $i = 0; $i <= $#sorted_members; $i++ ) {
	next if $param_indices{$sorted_members[$i]} == $i;
	return '[ ' . join(', ', map { '$_[' . $param_indices{$_} . ']' } @sorted_members) . ' ]';
    }
    return '[ @_ ]';
}
sub members {
    my $self = shift;
    return $self->SUPER::members(@_) if $#_ != 1;
    $self->SUPER::members(@_);
    my $overridden_class;
    if ( defined ($overridden_class = Class::Generate::Support::class_containing_method($_[0], $self)) ) {
	$self->{'member_index'}{$_[0]} = $overridden_class->{'member_index'}->{$_[0]};
    }
    else {
	$self->{'member_index'}{$_[0]} = ++$self->{'next_index'};
    }
}
sub index {
    my $self = shift;
    return '[' . $self->{'member_index'}{$_[0]} . ']';
}
sub last {
    my $self = shift;
    return $self->{'next_index'};
}
sub existence_test {
    my $self = shift;
    return 'defined';
}

sub size_establishment {
    my $self = shift;
    my $instance_var = $_[0];
    return '    $#' . $instance_var . ' = ' . $self->last . ";\n";
}
sub can_assign_all_params {
    my $self = shift;
    return ! $WARNING &&
	   $self->all_members_required &&
	   $self->constructor->style->isa('Class::Generate::Positional') &&
	   ! defined $self->parents;
}
sub undef_form {
    return 'undef';
}
sub wholesale_copy {
    return '[ @$self ]';
}
sub empty_form {
    return '[]';
}

package Class::Generate::Hash_Class;		# A subclass of Class defining
use vars qw(@ISA);				# hash-based classes.
use English;
@ISA = qw(Class::Generate::Class);

sub index {
    my $self = shift;
    return "{'" . $_[0] . "'}";
}
sub base {
    my $self = shift;
    return '{}' if ! $self->can_assign_all_params;
    my $style = $self->constructor->style;
    return '{ @_ }' if $style->isa('Class::Generate::Key_Value');
    my %order = $style->order;
    my $form = '{ ' . join(', ', map("$_ => \$_[$order{$_}]", keys %order));
    if ( $style->isa('Class::Generate::Mix') ) {
	$form .= ', @_[' . $style->pcount . '..$#_]';
    }
    return $form . ' }';
}
sub existence_test {
    return 'exists';
}
sub can_assign_all_params {
    my $self = shift;
    return ! $WARNING &&
	   $self->all_members_required &&
	   ! $self->constructor->style->isa('Class::Generate::Own') &&
	   ! defined $self->parents;
}
sub undef_form {
    return 'delete';
}
sub wholesale_copy {
    return '{ %$self }';
}
sub empty_form {
    return '{}';
}

package Class::Generate::Param_Style;		# A virtual class encompassing
use strict;					# parameter-passing styles.
use English;

sub new {
    my $class = shift;
    return bless {}, $class;
}

sub delete_self_members_form {
    shift;
    my @self_members = @_;
    if ( $#self_members == 0 ) {
	return q|delete $super_params{'| . $self_members[0] . q|'};|;
    }
    elsif ( $#self_members > 0 ) {
	return q|delete @super_params{qw(| . join(' ', @self_members) . q|)};|;
    }
}

sub odd_params_check_form {
    my $self = shift;
    my ($class, $constructor) = @_;
    return q|    croak '| . $constructor->name_form($class) . q|Odd number of parameters' if | .
			$self->odd_params_test($class) . ";\n";
}

sub my_decl_form {
    my $self = shift;
    my $class = $_[0];
    return '    my ' . $class->instance_var . ' = ' . $class->class_var . '->SUPER::new';
}

package Class::Generate::Key_Value;		# The key/value parameter-
use strict;					# passing style.  It adds
use vars qw(@ISA);				# the name of the variable
@ISA = qw(Class::Generate::Param_Style);	# that holds the parameters.
use English;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new;
    $self->{'holder'} = $_[0];
    return $self;
}

sub holder {
    my $self = shift;
    return $self->{'holder'};
}
sub ref {
    my $self = shift;
    return '$' . $self->holder . "{'" . $_[0] . "'}";
}
sub positional {
    return 0;
}
sub existence_test {
    return 'exists';
}
sub init_form {
    my $self = shift;
    my ($class, $constructor) = @_;
    my ($form, $cn);
    $form = '';
    $form .= $self->odd_params_check_form($class, $constructor) if $WARNING;
    $form .= "    my \%params = \@_;\n";
    return $form;
}
sub odd_params_test {
    return '$#_%2 == 0';
}
sub self_from_super_form {
    my $self = shift;
    my $class = $_[0];
    return '    my %super_params = %params;' . "\n" .
	   '    ' . $self->delete_self_members_form($class->nonprivate_member_names) . "\n" .
	   $self->my_decl_form($class) . "(\%super_params);\n";
}
sub params_check_form {
    my $self = shift;
    my ($class, $constructor) = @_;
    my ($cn, @nmns, $form);
    @nmns = $class->nonprivate_member_names;
    $cn = $constructor->name_form($class);
    $form = "    {\n\tmy \@unknown_params;\n";
    if ( ! @nmns ) {
	$form .= "\t\@unknown_params = keys \%params;\n";
    }
    elsif ( $#nmns == 0 ) {
	$form .= "\t\@unknown_params = grep \$_ ne '$nmns[0]', keys \%params;\n";
    }
    else {
	$form .= "\tmy %valid_param = (" . join(', ', map("'$_' => 1", @nmns)) . ");\n" .
		 "\t\@unknown_params = grep ! defined \$valid_param{\$_}, keys \%params;\n";
    }
    $form .= "\tcroak '$cn', join(', ', \@unknown_params), ': Not a member' if \@unknown_params;\n" .
	     "    }\n";
    return $form;
}

package Class::Generate::Positional;		# The positional parameter-
use strict;					# passing style.  It adds
use vars qw(@ISA);				# an ordering of parameters.
@ISA = qw(Class::Generate::Param_Style);

sub order {
    my $self = shift;
    return exists $self->{'order'} ? %{$self->{'order'}} : () if $#_ == -1;
    return exists $self->{'order'} ? $self->{'order'}->{$_[0]} : undef if $#_ == 0;
    $self->{'order'}->{$_[0]} = $_[1];
}
sub ref {
    my $self = shift;
    return '$_[' . $self->{'order'}->{$_[0]} . ']';
}
sub positional {
    return 1;
}
sub existence_test {
    return 'defined';
}
sub self_from_super_form {
    my $self = shift;
    my $class = $_[0];
    my $lb = scalar($class->nonprivate_member_names) || 0;
    return '    my @super_params = @_[' . $lb . '..$#_];' . "\n" .
	   $self->my_decl_form($class) . "(\@super_params);\n";
}
sub params_check_form {
    my $self = shift;
    my ($class, $constructor) = @_;
    my $cn = $constructor->name_form($class);
    my $max_params = scalar($class->nonprivate_member_names) || 0;
    return qq|    croak '$cn| . qq|Only $max_params parameter(s) allowed (', \$#_+1, ' given)'| .
		      " unless \$#_ < $max_params;\n";
}

package Class::Generate::Mix;			# The mix parameter-passing
use strict;					# style.  It combines key/value
use vars qw(@ISA);				# and positional.
@ISA = qw(Class::Generate::Param_Style);
use English;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new;
    $self->{'pp'} = Class::Generate::Positional->new;
    $self->{'kv'} = Class::Generate::Key_Value->new($_[0]);
    return $self;
}

sub order {
    my $self = shift;
    return $self->{'pp'}->order(@_) if $#_ <= 0;
    $self->{'pp'}->order(@_);
    $self->{'pnames'}{$_[0]} = 1;
}
sub ref {
    my $self = shift;
    return $self->{'pnames'}->{$_[0]} ? $self->{'pp'}->ref($_[0]) : $self->{'kv'}->ref($_[0]);
}
sub positional {
    my $self = shift;
    return $self->{'pnames'}->{$_[0]};
}
sub existence_test {
    my $self = shift;
    return $self->{'pnames'}->{$_[0]} ? $self->{'pp'}->existence_test : $self->{'kv'}->existence_test;
}
sub pcount {
    my $self = shift;
    return exists $self->{'pnames'} ? scalar(keys %{$self->{'pnames'}}) : 0;
}
sub init_form {
    my $self = shift;
    my ($class, $constructor) = @_;
    my ($form, $m) = ('', $self->max_possible_params($class));
    $form .= $self->odd_params_check_form($class, $constructor, $self->pcount, $m) if $WARNING;
    $form .= '    my %params = ' . $self->kv_params_form($m) . ";\n";
    return $form;
}
sub odd_params_test {
    my $self = shift;
    my $class = $_[0];
    my ($p, $test);
    $p = $self->pcount;
    $test = '$#_>=' . $p;
    $test .= ' && $#_<=' . $self->max_possible_params($class) if $class->parents;
    $test .= ' && $#_%2 == ' . ($p%2 == 0 ? '0' : '1');
    return $test;
}
sub self_from_super_form {
    my $self = shift;
    my $class = $_[0];
    my @positional_members = keys %{$self->{'pnames'}};
    my %self_members = map { ($_ => 1) } $class->nonprivate_member_names;
    delete @self_members{@positional_members};
    my $m = $self->max_possible_params($class);
    return $self->my_decl_form($class) . '(@_[' . ($m+1) . '..$#_]);' . "\n";
}
sub max_possible_params {
    my $self = shift;
    my $class = $_[0];
    my $p = $self->pcount;
    return $p + 2*(scalar($class->nonprivate_member_names) - $p) - 1;
}
sub params_check_form {
    my $self = shift;
    my $form = $self->{'kv'}->params_check_form(@_);
    if ( (my $i = index($form, 'qw(')) >= 0 ) {
	$i += 3;
	my (%names, $ll);
	$ll = index(substr($form, $i), ')');
	%names = map(($_ => 1), split /\s+/, substr $form, $i, $ll);
	delete @names{keys %{$self->{'pnames'}}};
	substr($form, $i, $ll) = join ' ', keys %names;
    }
    return $form;
}

sub kv_params_form {
    my $self = shift;
    my $max_params = $_[0];
    return '@_[' . $self->pcount . "..(\$#_ < $max_params ? \$#_ : $max_params)]";
}

package Class::Generate::Own;			# The "own" parameter-passing
use strict;					# style.
use vars qw(@ISA);
@ISA = qw(Class::Generate::Param_Style);

sub new {
    my $class = shift;
    my $self = $class->SUPER::new;
    $self->{'super_values'} = $_[0] if defined $_[0];
    return $self;
}

sub super_values {
    my $self = shift;
    return defined $self->{'super_values'} ? @{$self->{'super_values'}} : ();
}

sub can_assign_all_params {
    return 0;
}

sub self_from_super_form {
    my $self = shift;
    my $class = $_[0];
    my ($form, @sv);
    $form = $self->my_decl_form($class);
    if ( @sv = $self->super_values ) {
	$form .= '(' . join(',', @sv) . ')';
    }
    $form .= ";\n";
    return $form;
}

1;

# Copyright (c) 1999 Steven Wartik. All rights reserved. This program is free
# software; you can redistribute it and/or modify it under the same terms as
# Perl itself.
