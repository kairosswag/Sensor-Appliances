package dryr.base.bluetooth.chained_handler;

import java.util.List;
import java.util.LinkedList;

public class ChainedHandler<T> {
	protected List<SubHandler<T>> subHandlers;
	
	public ChainedHandler() {
		subHandlers = new LinkedList<SubHandler<T>>();
	}
	
	protected boolean subHandleFirst(T in) {
		for (SubHandler<T> subHandler: subHandlers) {
			if (subHandler.handle(in))
				return true;
		}
		
		return false;
	}
	
	protected boolean subHandleAll(T in) {
		boolean success = false;
		
		for (SubHandler<T> subHandler: subHandlers) {
			if (subHandler.handle(in))
				success = true;
		}
		
		return success;
	}	
}
