/**
 * @author Aras Bazyan
 * 
 * Class that communicates to Raspberry Pi over MQTT
 * 
 */

package mqtt;
import java.util.*;
import org.eclipse.paho.client.mqttv3.*;
import org.eclipse.paho.client.mqttv3.persist.*;

// url for Paho Client libraries in JAR: 
//https://repo.eclipse.org/content/repositories/paho/org/eclipse/paho/org.eclipse.paho.client.mqttv3/1.0.2/org.eclipse.paho.client.mqttv3-1.0.2.jar

public class Client {

   public static void main(String[] args) {

        int qos             = 0;
        String broker = "tcp://192.168.1.82:1883"; // RPi's IP
        String clientId     = "Aras";
        MemoryPersistence persistence = new MemoryPersistence();
        Scanner enterTopic = new Scanner(System.in);

 	while(true) {
           System.out.println("Enter topic name: ");
           String topic = enterTopic.nextLine();
           System.out.println("Enter message: ");
           String data = enterTopic.nextLine();
           //enterTopic.close();
           
        try {
            MqttClient sampleClient = new MqttClient(broker, clientId, persistence);
            MqttConnectOptions connOpts = new MqttConnectOptions();
            connOpts.setCleanSession(true);
            System.out.println("Connecting to broker: "+broker);
            sampleClient.connect(connOpts);
            System.out.println("Connected");
            System.out.println("Publishing message: "+data);
            MqttMessage message = new MqttMessage(data.getBytes());
            message.setQos(qos);
            sampleClient.publish(topic, message);
            System.out.println("Message published");
            sampleClient.disconnect();
            System.out.println("Disconnected");
           // System.exit(0);
        } catch(MqttException me) {
            System.out.println("reason "+me.getReasonCode());
            System.out.println("msg "+me.getMessage());
            System.out.println("loc "+me.getLocalizedMessage());
            System.out.println("cause "+me.getCause());
            System.out.println("excep "+me);
            me.printStackTrace();
        }
        }
   }
   }