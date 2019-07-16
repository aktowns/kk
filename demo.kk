# KK - a simple typesafe compile-to-yaml language that supports the C preprocessor
# This example is a kubernetes pod

%define metaName name =
  MetaObj { name = $name }

%include "kubernetes.kk"

Pod { 
  metadata = %metaName("busybox")
  spec     = PodSpec { 
    containers = [ Image { image   = "busybox"
                           command = ["sleep"]
                           imagePullPolicy = "IfNotPresent"
                           name = "busybox"
                         }
                 ]
  }
}
