from repoze.what.predicates import Predicate                                                                                           
# Googles ipaddr.py
from ipaddr import IPNetwork, IPv4Network, IPv6Network, IPAddress as IP
from types import ListType                                                                                                             

import logging
log = logging.getLogger(__name__)

class ip_from(Predicate):
    """ Only allow access to specified IPs through specified proxies"""

    message = 'Access denied for this IP' # Default message

    def __init__(self, allowed=None, proxies=None, message=None, **kwargs):
        """
        @param allowed: the ip or list of ips allowed to access the wsgi server
        @param proxy: the ip or list of ips of permitted proxies to access the
                      wsgi server through
        @param message: The message to return when check fails
        """

        allowed = [allowed] if type(allowed) is not ListType else allowed

        self.message = message if message else self.message
        self.allowed = []
        for address in allowed:
            if address is None:
                continue
            ip = None
            try:
                ip = IP(address)
            except ValueError:
                try:
                    ip = IPNetwork(address)
                except ValueError:
                    pass
            if ip:
                self.allowed.append(ip)

        # Allow proxy as a string
        if isinstance(proxies, (str, unicode)):
            # Convert to a list of one item
            proxies = [proxies]

        # Allow proxies as a list or tuple
        if isinstance(proxies, (list, tuple)):
            # Parse all proxies
            self.proxies = []
            for address in proxies:
                if address is None:
                    continue
                ip = None
                try:
                    ip = IP(address)
                except ValueError:
                    try:
                        ip = IPNetwork(address)
                    except ValueError:
                        pass
                if ip:
                    self.proxies.append(ip)
        else:
            self.proxies = bool(proxies)

        super(ip_from, self).__init__(**kwargs)

    def evaluate(self, environ, credentials):
        """
        check that the request ip and proxy are in the allowed lists
        @return: Boolean
        """

        # Take the ip from the environment
        remote_str = environ.get("REMOTE_ADDR")
        if remote_str:
            try:
                remote = IP(remote_str)
            except ValueError:
                self.unmet()
        if not remote_str or not remote:
            self.unmet()

        # Maybe REMOTE_ADDR holds proxy address. Try to find X_FORWARDED_FOR
        forwarded_str = environ.get('HTTP_X_FORWARDED_FOR')
        if forwarded_str:
            # remote did hold a proxy address. Assign it to proxy
            proxy = remote
            proxy_str = remote_str
            # And take the forwarded address
            remote_str = forwarded_str
            try:
                remote = IP(remote_str)
            except ValueError:
                self.unmet()
            if not remote:
                self.unmet()
        else:
            proxy = proxy_str = None

        msg = "Remote IP: %s Attempting Access" % remote
        if proxy:
            msg += ' Through %s' % proxy
        log.debug(msg)

        if proxy:
            # Check the proxy first
            if isinstance(self.proxies, (list, tuple)):
                # We have a list of allowed proxies. Iterate and try to find the
                # matching one
                valid = False
                for address in self.proxies:
                    if (isinstance(address, (IPv4Network, IPv6Network)) and \
                            proxy in address) or proxy == address:
                        log.debug("Proxy Validated")
                        valid = True
                        break
                if not valid:
                    # Not found
                    log.warn('Failed Access Attempt by %s through %s' %
                        (remote_str, proxy_str))
                    self.unmet('Access denied through this proxy')
            elif not self.proxies:
                # We do not accept proxies at all
                self.unmet('Access through proxies denied')

        # Now check the IP address
        valid = False
        # Iterate through the allowed list of addresses and try to find the
        # matching one
        for address in self.allowed:
            if (isinstance(address, (IPv4Network, IPv6Network)) and \
                    remote in address) or remote == address:
                log.debug("IP Validated")
                valid = True
                break

        if valid:
            # Store the IP (and proxy) in repoze identity
            identity = environ.get('repoze.who.identity')
            if not identity:
                # Create the identity if it does not exist
                identity = {}
                environ['repoze.who.identity'] = identity
            identity['ip'] = remote.compressed
            if proxy:
                identity['proxy'] = proxy.compressed
        else:
            msg = "Failed Access Attempt by %s" % remote_str
            if proxy_str is not None:
                msg += ' Through %s' % proxy_str
            log.warn(msg)

            self.unmet(self.message)

