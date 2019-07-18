## kk 

    kk v0.0.0, (C) Ashley Towns
  
    kk [OPTIONS] IN FILE
      to yaml or json configuration compiler
  
    Common flags:
         --outfile=OUT FILE     File to output, defaults to stdout
         --outformat=YAML|JSON  Format of the output file
      -? --help                 Display help message
      -V --version              Print version information
         --numeric-version      Print just the version number
      -v --verbose              Loud verbosity
      -q --quiet                Quiet verbosity
  
    compiles your kk file to either json or yaml


### Defining Types 

`%template` is used to define a type, for example a kubernetes pod could be represented as 

    %template MetaObj { name : String, labels : {String} }
    %template PodSpec { containers : [Image], retartPolicy : String }
    %template Image { image : String, command : [String], imagePullPolicy : String, name : String }
    %template Pod { metadata : MetaObj, spec : PodSpec }

and we can instantiate a type by providing it as an object name 

    MetaObj { name = "my-awesome-pod", labels = ["awesome"] }

in this case would compile to 
 
    name: my-awesome-pod
    labels: 
    - awesome

providing an invalid type for the object will result in a compile error.

along with enforcing types we can also provide hardcoded output for the resulting object
for example

    %template User { type = "User", firstName: String }
 
then create a User object 

    User { firstName = "Ashley" }

which results in

    type: User
    firstName: Ashley

### Intersection types 

We support intersection types for object building, that is to say
 
    %template A { firstName : String }
    %template B { lastName  : String }
 
    %template C = A & B  
 
would be the equivelent of 
 
    %template C { firstName : String, lastName : String }

and consist of the type `A & B`

### Union types 

We support union types aswell, for example we can have a Hash that contains either strings 
or numbers represented as `{String|Number}` or a list of Strings and Bools as `[String|Bool]`

### Optionality 

Optionality is represented as `?` for example 
 
    %template A { firstName : String? }
 
Optional fields are removed from the compiled output if a value is not provided. 
We can also provide a default value for an optional field like 
 
    %template A { firstName : String?("Bob") }
    %template B { hasSuperPowers : Bool?(false) }