using System;
using System.Collections.Generic;

// This could technically also be an interface, but it's not like we need
// to inherit from anything else
abstract class TaggedTree<B,L> {}

class TaggedNode<B,L> : TaggedTree<B,L> {
    public B tag;
    public List<TaggedTree<B,L>> children;

    public TaggedNode(B tag, List<TaggedTree<B,L>> children) {
        this.tag = tag;
        this.children = children; }}

class TaggedLeaf<B,L> : TaggedTree<B,L> {
    public L tag;

    public TaggedLeaf(L tag) {
        this.tag = tag; }}

// Can we implement a Linq interface for this tree corresponding to the
// haskell monad instance for tagged trees?
