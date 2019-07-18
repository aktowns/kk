# KK - a simple typed compile-to-yaml language
# This example is a kubernetes pod

%include "kubernetes.kk"

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
    containers = [ %image("busybox", ["sleep", 3600.0]) ]
    restartPolicy = "Always"
  }
}
