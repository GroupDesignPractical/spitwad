
var getMarkets = function($http)
{
  return $http(
    { url: '/markets'
    , method: 'GET'
    });
}



var getTrend_sources = function($http)
{
  return $http(
    { url: '/trend_sources'
    , method: 'GET'
    });
}



var getNews_sources = function($http)
{
  return $http(
    { url: '/news_sources'
    , method: 'GET'
    });
}



var getMarket = function($http, name, start, end)
{
  return $http(
    { url: '/market' + '?name=' + encodeURIComponent(name) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}



var getTrends = function($http, name, start, end)
{
  return $http(
    { url: '/trends' + '?name=' + encodeURIComponent(name) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}



var getNews = function($http, name, start, end)
{
  return $http(
    { url: '/news' + '?name=' + encodeURIComponent(name) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}
