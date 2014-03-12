#include <bindings.dsl.h>
#include <cvsu_graph.h>

module CVSU.Bindings.Graph where

#strict_import

import CVSU.Bindings.Types

import Foreign.Ptr
import Foreign.C.Types

#starttype graph_node
#field x , CDouble
#field y , CDouble
#field orientation , CDouble
#field scale , CDouble
#field attributes , <list>
#field neighbors , <list>
#field bonds , <list>
#stoptype

#starttype graph
#field vertices , <list>
#field edges , <list>
#field images , <list>
#stoptype
