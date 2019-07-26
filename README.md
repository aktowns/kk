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



### Syntax

KK syntax is centred around the creation of objects, a simple object looks like the following

```
MyObject {
    myObjectName = "hello"
}
```

this object maps to the following YAML 

```yml
myObjectName: hello
```

that's pretty boring, and ends up being longer than the resulting code, lets try something a little more complex, introducing the define keyword

```
%define defaultUserStatus = "awesome"
%define createUser(firstName, lastName) =
  User {
    kind = "User"
    firstName = %firstName
    lastName = %lastName
    status = %defaultUserStatus
  }

Admin {
    users = [%createUser("ashley", "towns"), %createUser("bob", "bobson")]
    capabilities = {edit: true, read: true}
}
```

This example we've introduced a new concept, functions! functions allow us to reuse objects without repeating our-self. The resulting YAML for the following was:

```yml
users:
  - kind: User
    firstName: ashley
    lastName: towns
    status: awesome
  - kind: User
    firstName: bob
    lastName: bobson
    status: awesome
capabilities:
  edit: true
  read: true
```

files containing multiple objects are separated by `---` though you can control the output by returning a single object that wraps the output in the way you expect. 

### Types

So far we've managed to get away with not using any types, as everything we've used so far was inferred. This is great for custom examples, but no good when we're trying to deploy against a well defined API/Schema. kk provides a `%template` keyword to define object types, kk's type system supports structural dependent typing for example a type may enforce the length of a field `[Char; 10]` is an array of at most 10 characters

### Environments

When you compile a kk file you can pass in user arguments via an environment, environments can be filled by command line with `-e key=value` or by the result of a command `-s command` an example of the first is giving unique names to a deployment, ie 

```
kk compile pod.kk -e name=myshell -e image=busybox -o busybox.yml
# then apply the resulting yml to a kubernetes cluster
kubectl apply -f busybox.yml
```

the second way could be used to fill the environment with a set of defaults, or things that you'd rather not store, for example if you have a command to retrieve login information for a database along with other defaults.

```
kk compile pod.kk -s "getenvironment -n default" -e name=mydatabase -o datastore.yml
kubectl apply -f busybox.yml
```

where the result of `getenvironment -n default` command is

```
DB_USERNAME=postgres
DB_PASSWORD=supersecure
DB_DATABASE=postgres
```

in this hypothetical example, the pod.kk would be handling secret by mapping it to a kubernetes secret object. 

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