/*                                                                        */
/* (c) Copyright IBM Corp. 2020 All Rights Reserved                       */
/*                                                                        */

package com.ibm.cicsdev.springboot.link.app.ui.mvc;

import javax.validation.Valid;

import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.ibm.cicsdev.springboot.link.app.ui.Message;
import com.ibm.cicsdev.springboot.link.app.ui.MessageRepository;


@Controller
@RequestMapping("/")
public class MessageController 
{
	private final MessageRepository messageRepository;

	
	/**
	 * @param messageRepository
	 */
	public MessageController(MessageRepository messageRepository) 
	{
		this.messageRepository = messageRepository;
	}
	
	
	/**
	 * @return list all messages
	 */
	@GetMapping
	public ModelAndView list() 
	{
		Iterable<Message> messages = this.messageRepository.findAll();
		return new ModelAndView("messages/list", "messages", messages);
	}

	
	/**
	 * @param message 
	 * @return message with an id
	 */	
	@GetMapping("{id}")
	public ModelAndView view(@PathVariable("id") Message message) {
		return new ModelAndView("messages/view", "message", message);
	}

	
	/**
	 * @param message
	 * @return a form to create a new message from the web UI
	 */
	@GetMapping("form")
	public String createForm(@ModelAttribute Message message) 
	{
		return "messages/form";
	}

	
	/**
	 * @param message
	 * @param result
	 * @param redirect
	 * @return a new message with a new id after execute Create Message
	 */
	@PostMapping
	public ModelAndView create(@Valid Message message, BindingResult result, RedirectAttributes redirect) 
	{
		if (result.hasErrors()) 
		{
			return new ModelAndView("messages/form", "formErrors", result.getAllErrors());
		}
		message = this.messageRepository.save(message);
		redirect.addFlashAttribute("globalMessage", "view.success");
		return new ModelAndView("redirect:/{message.id}", "message.id", message.getId());
	}

	
	/**
	 * @return Whitelabel Error Page
	 */
	@RequestMapping("foo")
	public String foo() {
		throw new RuntimeException("Expected exception in controller");
	}

	
	/**
	 * @param id
	 * @return all messages except the deleted message with an id
	 */
	@GetMapping("delete/{id}")
	public ModelAndView delete(@PathVariable("id") Long id) 
	{
		this.messageRepository.deleteMessage(id);
		Iterable<Message> messages = this.messageRepository.findAll();
		return new ModelAndView("messages/list", "messages", messages);
	}

	
	/**
	 * @param message
	 * @return a form for modifying the message with an id
	 */
	@GetMapping("modify/{id}")
	public ModelAndView modifyForm(@PathVariable("id") Message message) 
	{
		return new ModelAndView("messages/form", "message", message);
	}

}
