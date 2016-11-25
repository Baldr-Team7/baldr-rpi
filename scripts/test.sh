#! /bin/bash
mosquitto_pub -h tann.si -p 8883 -t lightcontrol/home/asdf/light/1/commands -f ./scripts/test_message.json