/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2019 All Rights Reserved                       */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */
package com.ibm.cicsdev.springboot.link.app.ui.cics;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.ibm.cics.server.CCSIDErrorException;
import com.ibm.cics.server.ChannelErrorException;
import com.ibm.cics.server.CodePageErrorException;
import com.ibm.cics.server.ContainerErrorException;
import com.ibm.cics.server.Task;
import com.ibm.cics.server.invocation.CICSProgram;
import com.ibm.cicsdev.springboot.link.app.ui.Message;
import com.ibm.cicsdev.springboot.link.app.ui.MessageRepository;

/**
 * @author Matthew Willson, Ivan Hargreaves, Anneli
 *
 */
@Component
public class CICSCallable {

	
	@Autowired
	private MessageRepository messageRepo;

	/**
	 * Take a message passed from CICS and save it in the MessageRespository.
	 * The message is passed on the current channel in a container called MESSAGE.
	 *  
	 * After saving the message it should be visible from the web UI provided by this application.
	 * 
	 * @throws ContainerErrorException
	 * @throws ChannelErrorException
	 * @throws CCSIDErrorException
	 * @throws CodePageErrorException
	 */
	@CICSProgram(value ="YOSPRING") // This method can be invoked by EXEC CICS LINK PROGRAM(YOSPRING)
	public void callMeFromCICS() throws ContainerErrorException, ChannelErrorException, CCSIDErrorException, CodePageErrorException {
		
		String messageText = Task.getTask().getCurrentChannel().getContainer("MESSAGE").getString();
		
		Message msg = new Message();
		msg.setSummary("Message from CICS");
		msg.setText(messageText);
		this.messageRepo.save(msg);
	}

}
