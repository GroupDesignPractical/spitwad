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
getStock: function(symbol, start, end)
{
  return $http(
    { url: '/stock' + '?symbol=' + encodeURIComponent(symbol) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}
,
getTrends: function(source, start, end)
{
  return $http(
    { url: '/trends' + '?source=' + encodeURIComponent(source) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}
,
getNews: function(source, start, end)
{
  return $http(
    { url: '/news' + '?source=' + encodeURIComponent(source) + '&start=' + encodeURIComponent(start) + '&end=' + encodeURIComponent(end)
    , method: 'GET'
    });
}
,
postUpdate_stocks: function(symbol, since)
{
  return $http(
    { url: '/update_stocks' + '?symbol=' + encodeURIComponent(symbol) + '&since=' + encodeURIComponent(since)
    , method: 'POST'
    });
}
,
postUpdate_news: function(source)
{
  return $http(
    { url: '/update_news' + '?source=' + encodeURIComponent(source)
    , method: 'POST'
    });
}
});
});
