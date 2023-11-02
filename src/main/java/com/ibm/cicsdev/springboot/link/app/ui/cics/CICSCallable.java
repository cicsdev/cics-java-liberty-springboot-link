/*                                                                        */
/* (c) Copyright IBM Corp. 2020 All Rights Reserved                       */
/*                                                                        */
package com.ibm.cicsdev.springboot.link.app.ui.cics;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Channel;
import com.ibm.cics.server.invocation.CICSProgram;
import com.ibm.cicsdev.springboot.link.app.ui.Message;
import com.ibm.cicsdev.springboot.link.app.ui.MessageRepository;

@Component
public class CICSCallable {
    @Autowired
    private MessageRepository messageRepo;
    
    private static final String INPUT_CONTAINER = "MESSAGE"; // Name of CICS container used for input
    private static final String CICS_PROGRAM = "YOSPRING";   // Name of CICS program entry point


    /**
     * Take a message passed from CICS and save it in the MessageRespository. 
     * The message is passed on the current channel in a container defined by INPUT_CONTAINER.
     * 
     * This method can be invoked by the following CICS commands
     * EXEC CICS PUT CONTAINER(MESSAGE) CHANNEL(xx) FROM("HELLO") 
     * EXEC CICS LINK PROGRAM(YOSPRING) CHANNEL(xx)
     * 
     * If no CONTAINER is provided on the link then a default message is created
     * 
     * After saving the message it should be visible from the web UI provided by this application.
     * 
     * @throws CicsConditionException
     */

    @CICSProgram(value = CICS_PROGRAM)
    public void callMeFromCICS() throws CicsConditionException {
        
        String messageText;

        // Get the CICS task, and the current channel        
        Channel chan = Task.getTask().getCurrentChannel();
        
        // If no channel or container preset the message
        if (chan == null) {
            messageText = "NO CHANNEL";
        } else {
            // Get the input string from the input container if it was passed in
            Container cont = chan.getContainer(INPUT_CONTAINER);
            if (cont != null) {
                messageText = cont.getString();
            } else {
                messageText = "NO CONTAINER";
            }
        }

        // Build the message to add to the repository 
        Message msg = new Message();
        msg.setSummary("Message from CICS");
        msg.setText(messageText);
        this.messageRepo.save(msg);
    }

}
