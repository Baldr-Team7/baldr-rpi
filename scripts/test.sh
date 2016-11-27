#! /bin/bash

LIGHT_ID=1

publish_path(){
	mosquitto_pub -h tann.si -p 8883 -t lightcontrol/home/asdf/light/$LIGHT_ID/commands -f $1	
}

clear

while true; do
	echo "What test do you want to run"
	echo "1. Set all to [on, #0FF0FF, Kitchen]"
	echo "2. Set room to Living Room"
	echo "3. Set color to #0000FF"
	echo "4. Turn light off"
	echo "5. -"
	echo "6. -"
	echo "7. -"
	echo "s  Set light ID"
	echo "e  Exit"
	echo -n $'\n\n:'

	read selection
		
	case $selection in
		1)
		publish_path "./scripts/test_message_set.json"
		;;
		2)
		publish_path "./scripts/test_message_room.json"
		;;
		3)
		publish_path "./scripts/test_message_color.json"
		;;
		4)
		publish_path "./scripts/test_message_state.json"
		;;
		s)
		clear
		echo -n $'\nEnter light ID:'
		read LIGHT_ID
		;;
		e)
		break 2
		;;
	esac


	echo $'\nDone.'
	sleep 1
	clear
done
