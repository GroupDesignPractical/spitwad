app.service('spitwad', function($http) {
  return ({
getStocks: function()
{
  return $http(
    { url: '/stocks'
    , method: 'GET'
    });
}
,
getTrend_sources: function()
{
  return $http(
    { url: '/trend_sources'
    , method: 'GET'
    });
}
,
getNews_sources: function()
{
  return $http(
    { url: '/news_sources'
    , method: 'GET'
    });
}
,
getStock: function(name, start, end)
{
  return $http(
    { url: '/stock' + '?name=' + encodeURIComponent(name) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}
,
getTrends: function(name, start, end)
{
  return $http(
    { url: '/trends' + '?name=' + encodeURIComponent(name) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}
,
getNews: function(name, start, end)
{
  return $http(
    { url: '/news' + '?name=' + encodeURIComponent(name) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}
});
});
