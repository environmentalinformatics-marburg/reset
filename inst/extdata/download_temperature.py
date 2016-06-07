#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "ei",
    "dataset": "interim",
    "date": "2013-01-01/to/2013-01-31",
    "expver": "1",
    "grid": "0.125/0.125",
    "levelist": "400/450/500/550/600/650/700/750/775/800/825/850/875/900/925/950/975/1000",
    "levtype": "pl",
    "param": "130.128",
    "step": "0",
    "stream": "oper",
    "area": "-2/37/-4/38",
    "target": "2013001_2013031__temperature.nc",
    "time": "12",
    "type": "an",
})
