package carma.ui.jrtd.ui;

public final class MyNetworkException extends RuntimeException {
    private final Exception ex;
    private final String extra;

    public MyNetworkException(final String message, final String extra, final Exception ex) {
        super(message);
        this.ex = ex;
        this.extra = extra;
    }

    public MyNetworkException(final String message, final Exception ex) {
        this(message, null, ex);
    }

    public MyNetworkException(final String message, final String extra) {
        this(message, extra, null);
    }

    public MyNetworkException(final String message) {
        this(message, null, null);
    }

    public Exception getException() {
        return this.ex;
    }

    public String getExtra() {
        return this.extra;
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
