/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2020 All Rights Reserved                       */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */


package com.ibm.cicsdev.springboot.link.app.ui;

/**
 * @author Matthew Willson, Ivan Hargreaves
 *
 */
public interface MessageRepository {

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
