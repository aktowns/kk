# KK - a simple typesafe compile-to-yaml language
# This example is a kubernetes pod

%template MetaObj {
  name   : String
  labels : {String}
}

%template PodSpec {
  containers   : [Image]
  retartPolicy : String
}

%template Image {
  image           : String
  command         : [String]
  imagePullPolicy : String
  name            : String
}

%template Pod {
  kind     = "Pod"
  metadata : MetaObj
  spec     : PodSpec
}

# An example function, that generates an image block
%define image(name, commands) =
  Image { image = $name
          command = $commands
          imagePullPolicy = "IfNotPresent"
          name = $name
        }

# Lets make a pod
Pod {
  metadata = MetaObj { name   = "busybox"
                       labels = { app: "busybox" }
                     }
  spec     = PodSpec {
    containers = [ %image("busybox", ["sleep", "3600"]) ]
    restartPolicy = "Always"
  }
}
