// $Id: Cache.java,v 1.3 2014/08/26 17:56:04 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.lang.Iterable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.NoSuchElementException;

/**
 * Class used to cache any type of data. It will automatically remove
 * data in least-recently-added order as new data is added.
 *
 * When iterating, data is presented in older-to-newer order.
 *
 * NOTE: Not thread safe! You must protect this class externally.
 */
public class Cache<T> implements Iterable {
	private final int size_;
	private final LinkedList<T> cache_ = new LinkedList<T>();

	public Cache(int size) {
		this.size_ = size;
	}

	private void trim() {
		while (this.cache_.size() > this.size_)
			this.cache_.removeFirst();
	}

	public void add(T t) {
		this.cache_.addLast(t);
		this.trim();
	}

	// get items by index: OLDEST DATA FIRST
	public T get(int index) {
		return this.cache_.get(index);
	}

	// get the newest data inserted in the cache
	public T getLast() {
		try {
			return this.cache_.getLast();
		} catch (NoSuchElementException ex) {
			return null;
		}
	}

	// alias for getLast()
	public T getNewest() {
		return this.getLast();
	}

	// replace the last item added to the cache
	public void replace(T t) {
		if (!this.cache_.isEmpty())
			this.cache_.removeLast();

		this.cache_.addLast(t);
		this.trim();
	}

	// iterate over the cache: OLDEST DATA FIRST
	public Iterator<T> iterator() {
		return this.cache_.iterator();
	}

	// iterate over the cache: NEWEST DATA FIRST
	public Iterator<T> descendingIterator() {
		return this.cache_.descendingIterator();
	}

	public int size() {
		return this.cache_.size();
	}

	public T[] toArray(T[] a) {
		return this.cache_.toArray(a);
	}
}
