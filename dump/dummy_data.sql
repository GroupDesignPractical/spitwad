BEGIN TRANSACTION;
INSERT INTO "stock" VALUES ('HSBA', 'HSBC Holdings plc', 0, '', ''), ('LLOY', 'Lloyds Banking Group plc', 0, '', '');
INSERT INTO "trend_source" VALUES ('Facebook'), ('Twitter');
INSERT INTO "news_source" VALUES ('BBC News', 'https://www.facebook.com/bbcnews'), ('Reuters', 'https://www.facebook.com/Reuters');
INSERT INTO "stock_data" ("stock_symbol", "date", "datum")
  VALUES
  ('LLOY', '2017-03-21T21:45:34.924820526', '1.5'),
  ('LLOY', '2017-03-22T21:46:28.141110199', '1.2'),
  ('HSBA', '2017-03-23T18:38:21.232719831', '2.5'),
  ('HSBA','2017-03-23T21:22:28.141110199', '1.5'),
  ('LLOY','2017-03-23T14:23:28.198721371', '4.3');
INSERT INTO "news_data" VALUES
  ('BBC News', '2017-03-04T00:00:00', '''Defend NHS with all your might'', Corbyn urges demonstrators', 'https://www.theguardian.com/society/2017/mar/04/nhs-funding-cuts-protest-march-london-jeremy-corbyn', 0, 0, 0, 0, 0, 0),
  ('BBC News', '2017-03-17T00:00:00', 'Fleeing from Dante''s hell on Mount Etna', 'https://www.theguardian.com/world/2017/mar/17/fleeing-from-dantes-hell-on-mount-etna', 0, 0, 0, 0, 0, 0);
COMMIT;
