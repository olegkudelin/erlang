#!/bin/bash
curl -XPOST -d "{}" http://127.0.0.1:8888/sequence/create
curl -H "Content-Type: application/json" -X POST -d "{\"observation\": {\"color\": \"green\",\"numbers\": [\"1110111\", \"0011101\"]},\"sequence\": \"718ec2f0-f023-4b91-bc88-415c60186c82\"}" http://127.0.0.1:8888/observation/add
curl -H "Content-Type: application/json" -X POST -d "{\"observation\": {\"color\": \"green\",\"numbers\": [\"1110111\", \"0010000\"]},\"sequence\": \"718ec2f0-f023-4b91-bc88-415c60186c82\"}" http://127.0.0.1:8888/observation/add
curl -H "Content-Type: application/json" -X POST -d "{\"observation\": {\"color\": \"red\"},\"sequence\": \"718ec2f0-f023-4b91-bc88-415c60186c82\"}" http://127.0.0.1:8888/observation/add
curl -H "Content-Type: application/json" -X POST -d "{\"observation\": {\"color\": \"green\",\"numbers\": [\"1110111\", \"0010000\"]},\"sequence\": \"718ec2f0-f023-4b91-bc88-415c60186c82\"}" http://127.0.0.1:8888/observation/add
curl http://127.0.0.1:8888/clear
