#! /bin/bash

LIGHT_ID=1
HOME_ID=asdf
WAIT=0
publish_path(){
	TOPIC=lightcontrol/home/$HOME_ID/light/$LIGHT_ID/commands
	echo $'\n\nSending\n\n'
	echo "Topic: "$TOPIC
	echo $'Message:\n'
	cat $1

	mosquitto_pub -h tann.si -p 8883 -t $TOPIC -f $1	


	echo $'\n\nDone.'

	if [ $WAIT == 1 ] 
		then
			echo 'Enter to continue...'
			read 
		else
			sleep 1
	fi
}

publish_string(){
	TOPIC=lightcontrol/home/$HOME_ID/light/$LIGHT_ID/commands
	echo $'\n\nSending\n\n'
	echo "Topic: "$TOPIC
	echo $'Message:\n' $1

	mosquitto_pub -h tann.si -p 8883 -t $TOPIC -m $1


	echo $'\n\nDone.'

	if [ $WAIT == 1 ] 
		then
			echo 'Enter to continue...'
			read 
		else
			sleep 1
	fi
}

color_message(){
	MESSAGE='{"protocolName":"baldr","version":1,"lightCommand":{"color":"'$1'"}}'
}

clear
color_message "test"
echo $MESSAGE

while true; do
	echo "What test do you want to run"
	echo
	echo "1. Set to [on, #0FF0FF, Kitchen]"
	echo "2. Set room to Living Room"
	echo "3. Turn light on"
	echo "4. Turn light off"
	echo "5. Set color to red"
	echo "6. Set color to green"
	echo "7. Set color to blue"
	echo "8. Set color to yellow"
	echo "9. Set color to purple"
	echo 
	echo "l  Set light ID"
	echo "h  Set home ID"
	echo "e  Exit"
	echo -n $'\n\n:'

	read -n1 selection
		
	case $selection in
		1)
		publish_path "./scripts/test_message_all.json"
		;;
		2)
		publish_path "./scripts/test_message_room.json"
		;;
		3)
		publish_path "./scripts/test_message_on.json"
		;;
		4)
		publish_path "./scripts/test_message_off.json"
		;;
		5)
		color_message "#FF0000"
		publish_string $MESSAGE
		;;
		6)
		color_message "#00FF00"
		publish_string $MESSAGE
		;;
		7)
		color_message "#0000FF"
		publish_string $MESSAGE
		;;
		8)
		color_message "#FFFF00"
		publish_string $MESSAGE
		;;
		9)
		color_message "#FF00FF"
		publish_string $MESSAGE
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
	clear
done
