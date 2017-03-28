INSERT INTO "market" ("name") VALUES ('Crude Oil'), ('Dow Jones'), ('FTSE 100');
INSERT INTO "trend_source" ("name") VALUES ('Facebook'), ('Twitter');
INSERT INTO "news_source" ("name") VALUES ('BBC News'), ('Reuters');
INSERT INTO "market_data" ("market_name", "date", "datum")
  VALUES ('Crude Oil', '2017-03-21T21:45:34.924820526', '1.5');
INSERT INTO "market_data" ("market_name", "date", "datum")
  VALUES ('Crude Oil', '2017-03-22T21:46:28.141110199', '1.2');
INSERT INTO "market_data" ("market_name", "date", "datum")
  VALUES ('Crude Oil', '2017-03-23T18:38:21.232719831', '2.5');
INSERT INTO "market_data" ("market_name", "date", "datum")
  VALUES ('Crude Oil','2017-03-23T21:22:28.141110199', '1.5');
INSERT INTO "market_data" ("market_name", "date", "datum")
  VALUES ('Crude Oil','2017-03-23T14:23:28.198721371', '4.3');
INSERT INTO "news_data" VALUES
  ('BBC News', '2017-03-04T00:00:00', '''Defend NHS with all your might'', Corbyn urges demonstrators', 'https://www.theguardian.com/society/2017/mar/04/nhs-funding-cuts-protest-march-london-jeremy-corbyn'),
  ('BBC News', '2017-03-17T00:00:00', 'Fleeing from Dante''s hell on Mount Etna', 'https://www.theguardian.com/world/2017/mar/17/fleeing-from-dantes-hell-on-mount-etna');
