package dsscratch.algos.snapshots

import dsscratch.algos.nodes.Node
import dsscratch.clocks.TimeStamp

case class Snapshot(id: TimeStamp, nd: Node)