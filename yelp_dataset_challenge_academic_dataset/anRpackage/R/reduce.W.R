reduce.W <-
function(original.graph, new.graph, oldW)
{
    num.users.original <- length(V(original.graph)[V(original.graph)$isuser])
    objects.original <- V(original.graph)[!V(original.graph)$isuser]

    objects.new <- V(new.graph)[!V(new.graph)$isuser]
    num.users.new <- length(V(new.graph)[V(new.graph)$isuser])
    sd <- setdiff(objects.original, objects.new)-num.users.original
}
