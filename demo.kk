# KK - a simple typed compile-to-yaml language
#
# This example is a kubernetes pod, customized by passed in arguments

%include "kubernetes.kk"

%define name  = %env:name
%define image = %env:image

#---------------------------------------

%define appname = "pod-%{name}"

%define IfNotPresent = "IfNotPresent"

# An example function, that generates an image block
%define podImage(name, commands) =
  Image { image = %image
          command = %commands
          imagePullPolicy = %IfNotPresent
          name = %name
        }

# Lets make a pod
Pod {
  metadata = MetaObj { name   = %appname
                       labels = { app: %image }
                     }
  spec     = PodSpec {
    containers = [ %podImage("busybox", ["sleep", 3600.0]) ]
    restartPolicy = "Always"
  }
}
