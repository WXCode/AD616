remove(list=ls())
library(R6)
library(data.tree)
'%ni%' <- Negate('%in%')
Tree <- R6Class("Decision Trees", list(
  name = NULL,
  root = NULL,
  GetNodeLabel = NULL,
  GetNodeShape = NULL,
  node_list = c(),
  initialize = function(name,type='decision') {
    stopifnot(is.character(name), length(name) == 1)
    self$name <- name
    self$node_list[name] = NA
    self$node_list[name] = list(Node$new(name,type=type))
    self$root = self$node_list[name]
    self$GetNodeShape= function(node) switch(node$type, decision = "box", chance = "ellipse",branch='none',terminal='none')
    self$GetNodeLabel=  function(node) switch(node$type, terminal=node$payoff, chance=node$name,decision=node$name,branch=node$name)

  },
  get_nodes =function(){
    return(self$node_list)
  },
  plot_graph = function(){
    GetEdgeLabel <- function(node) {
      if (!node$isRoot && node$parent$type == 'chance') {
        label = paste0(node$name, " (", node$p, ")")
      } else if (!node$isRoot && node$parent$type == 'decision'){
        label = paste0(node$route)
      }else {
        label = ""
      }
      return (label)
    }

    SetEdgeStyle(self$root[[1]], fontname = 'helvetica',fontsize=18, label = GetEdgeLabel,color='blue')
    SetNodeStyle(self$root[[1]], fontname = 'helvetica', fontsize=18,shape = self$GetNodeShape, label = self$GetNodeLabel)

    plot(self$root[[1]])
  },
  addDecision = function(pNode,name,prob,route=''){
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.character(pNode), length(pNode) == 1)
    stopifnot(is.numeric(prob), length(prob) == 1)
    stopifnot(name %ni% colnames(df))
    print(self$node_list[pNode])
    self$node_list[name] <- list(self$node_list[pNode][[1]]$AddChild(name, type = 'decision',route = route,p=prob))
  },
  addChance = function(pNode,name,cost,route=''){
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.character(pNode), length(pNode) == 1)
    stopifnot(is.numeric(cost), length(cost) == 1)
    stopifnot(name %ni% colnames(df))
    self$node_list[name] <- list(self$node_list[pNode][[1]]$AddChild(name, type = 'chance',route = route,cost=cost))
  },
  addTerminal = function(pNode,name,prob,payoff){
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.character(pNode), length(pNode) == 1)
    stopifnot(is.numeric(payoff), length(payoff) == 1)
    stopifnot(is.numeric(prob), length(prob) == 1)
    stopifnot(name %ni% colnames(df))
    self$node_list[name] <- list(self$node_list[pNode][[1]]$AddChild(name, type = 'terminal',payoff=payoff,p=prob))
  },
  update_payoff = function(){
    payoff <- function(node) {
      if (node$type == 'chance') node$payoff = node$cost + sum(sapply(node$children, function(child) child$payoff * child$p))
      else if (node$type == 'decision') {
        print (node$children)
        node$payoff <-  max(sapply(node$children, function(child) child$payoff))
      }
    }
    self$root[[1]]$Do(payoff, traversal = "post-order", filterFun = isNotLeaf)
    self$GetNodeLabel= function(node) switch(node$type, terminal=node$payoff, chance=paste("EV =",node$payoff),decision=paste(node$name,"EMV =",node$payoff),branch=node$name)
    SetNodeStyle(self$root[[1]], fontname = 'helvetica', fontsize=18,shape = self$GetNodeShape, label = self$GetNodeLabel)
  },
  update_utility = function(R){
    payoff <- function(node) {
      if(node$type == 'terminal') node$payoff <- round((1-exp(-node$payoff/R)),4)
      else if (node$type == 'chance') node$payoff <- node$cost + sum(sapply(node$children, function(child) child$payoff * child$p))
      else if (node$type == 'decision') node$payoff <-  max(sapply(node$children, function(child) child$payoff))
    }
    self$root[[1]]$Do(payoff, traversal = "post-order",filterFun = isLeaf)
    self$root[[1]]$Do(payoff, traversal = "post-order", filterFun = isNotLeaf)
    self$GetNodeLabel<- function(node) switch(node$type, terminal=node$payoff, chance=paste("EV =",node$payoff),decision=paste(node$name,"EMV =",node$payoff),branch=node$name)
    SetNodeStyle(self$root[[1]], fontname = 'helvetica', fontsize=18,shape = self$GetNodeShape, label = self$GetNodeLabel)
  }
  )
)

b <- Tree$new('VentureCapital',type='decision')
b$addChance(name = 'test1',pNode = 'VentureCapital',cost = 0,route = "yes")
b$addTerminal(pNode = 'test1',name = 'not_good',prob = 0.5,payoff = 110)
b$addTerminal(pNode = 'test1',name = 'good',prob = 0.5,payoff = 100)


b$plot_graph()
b$update_payoff()
