var _Janiczek$middleware$Native_Middleware_Navigation = function() {

// GET LOCATION

function getLocation()
{
	var location = document.location;

	return {
		href: location.href,
		host: location.host,
		hostname: location.hostname,
		protocol: location.protocol,
		origin: location.origin,
		port_: location.port,
		pathname: location.pathname,
		search: location.search,
		hash: location.hash,
		username: location.username,
		password: location.password
	};
}


// DETECT IE11 PROBLEMS

function isInternetExplorer11()
{
	return window.navigator.userAgent.indexOf('Trident') !== -1;
}


return {
	getLocation: getLocation,
	isInternetExplorer11: isInternetExplorer11
};

}();
