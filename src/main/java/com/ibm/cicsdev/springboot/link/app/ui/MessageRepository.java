/*                                                                        */
/* (c) Copyright IBM Corp. 2020 All Rights Reserved                       */
/*                                                                        */


package com.ibm.cicsdev.springboot.link.app.ui;

public interface MessageRepository 
{
	/**
	 * @return Iterable<Message>, all messages
	 */
	Iterable<Message> findAll();

	/**
	 * @param message received
	 * @return saved message
	 */
	Message save(Message message);

	/**
	 * @param id
	 * @return the message by id
	 */
	Message findMessage(Long id);

	/**
	 * @param id
	 */
	void deleteMessage(Long id);
}
