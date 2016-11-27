#! /bin/bash

LIGHT_ID=1
HOME_ID=asdf
publish_path(){
	mosquitto_pub -h tann.si -p 8883 -t lightcontrol/home/$HOME_ID/light/$LIGHT_ID/commands -f $1	
}

clear

while true; do
	echo "What test do you want to run"
	echo
	echo "1. Set to [on, #0FF0FF, Kitchen]"
	echo "2. Set room to Living Room"
	echo "3. Set color to #0000FF"
	echo "4. Turn light off"
	# echo "5. -"
	# echo "6. -"
	# echo "7. -"
	# echo "8. -"
	# echo "9. -"
	echo 
	echo "l  Set light ID"
	echo "h  Set home ID"
	echo "e  Exit"
	echo -n $'\n\n:'

	read -n1 selection
		
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
		l)
		clear
		echo -n $'\nEnter light ID:'
		read LIGHT_ID
		;;
		h)
		clear
		echo -n $'\nEnter home ID:'
		read HOME_ID
		;;
		e)
		echo 
		break 2
		;;
	esac


	echo $'\n\nDone.'
	sleep 1
	clear
done
