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