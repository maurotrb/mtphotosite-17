WITH RECURSIVE
-- Extract public tags (Uppercase tags are private)
PublicTags(tagid, tagname) AS
(
    SELECT TP.id,
        CASE
            WHEN UPPER(TP.NAME) = TP.NAME THEN ''
            ELSE TP.NAME
        END
    FROM   Tags TP
    WHERE  TP.pid = 0 -- Root tags
    AND    (TP.NAME = 'WHAT?'
	OR    TP.NAME = 'WHEN?'
	OR    TP.NAME = 'WHERE?'
    OR    TP.NAME = 'WHY?')
    UNION ALL
    SELECT  T.id,
            CASE
                 WHEN UPPER(T.NAME) = T.NAME
                 AND     PublicTags.tagname = '' THEN ''
                 WHEN UPPER(T.NAME) = T.NAME THEN PublicTags.tagname
                 WHEN PublicTags.tagname = '' THEN T.NAME
                 ELSE PublicTags.tagname || '|' || T.NAME
            END
    FROM    Tags T
    INNER JOIN PublicTags ON T.pid = PublicTags.tagid),
-- Associate tags to images and flatten tags per image
ImagePublicTags (imageid, groupedtags) AS
(
        SELECT IT.imageid,
               group_concat(PT.tagname,'|')
        FROM   ImageTags IT
            INNER JOIN PublicTags PT ON PT.tagid = IT.tagid
        WHERE    PT.tagname <> ''
        GROUP BY IT.imageid),
-- Extract gallery tags
GalleryTags(tagid, tagname) AS
(
    SELECT TP.id,
        CASE
            WHEN UPPER(TP.NAME) = TP.NAME THEN ''
            ELSE TP.NAME
        END
    FROM   Tags TP
    WHERE  TP.NAME = 'MTPHOTO.COM'
    UNION ALL
    SELECT     T.id,
            CASE
                 WHEN UPPER(T.NAME) = T.NAME AND GalleryTags.tagname = '' THEN ''
                 WHEN UPPER(T.NAME) = T.NAME THEN GalleryTags.tagname
                 WHEN GalleryTags.tagname = '' THEN T.NAME
                 ELSE GalleryTags.tagname || '|' || T.NAME
            END
    FROM    Tags T
    INNER JOIN GalleryTags ON T.pid = GalleryTags.tagid),
-- Associate gallery tags to images and flatten tags per image
ImageGalleryTags (imageid, groupedtags) AS
(
        SELECT  IT.imageid,
                group_concat(GT.tagname,'|')
        FROM    ImageTags IT
        INNER JOIN GalleryTags GT ON GT.tagid = IT.tagid
        GROUP BY   IT.imageid),
-- Extract Title
ImageTitle (imageid, title) AS
(
    SELECT IC.imageid,
           REPLACE(REPLACE(IC.comment, CHAR(13), ''), CHAR(10), '')
    FROM   ImageComments IC
    WHERE  IC.type = 3),
-- Extract Caption
ImageCaption (imageid, caption) AS
(
    SELECT IC.imageid,
           REPLACE(REPLACE(IC.comment, CHAR(13), ''), CHAR(10), '')
    FROM   ImageComments IC
    WHERE  IC.type = 1)
SELECT    REPLACE(I.NAME, '.' || REPLACE(I.NAME, RTRIM(I.NAME, REPLACE(I.NAME, '.', '')), ''), '') AS image,
          II.creationDate AS capture_time,
          IP.latitudeNumber AS gps_latitude,
          IP.longitudeNumber AS gps_longitude,
          ITIT.title,
          ICAP.caption,
          strftime('%Y-%m-%d', II.creationDate)
                    || '|'
                    || strftime('%Y-%m', II.creationDate)
                    || '|'
                    || strftime('%Y', II.creationDate)
                    ||
               CASE
                    WHEN IPT.groupedtags = ''
                    OR        IPT.groupedtags IS NULL THEN ''
                    ELSE '|'
                                     || IPT.groupedtags
               END AS export_tags,
          strftime('%Y Timeline', II.creationDate)
                    ||
               CASE
                    WHEN IGT.groupedtags = ''
                    OR        IGT.groupedtags IS NULL THEN ''
                    ELSE '|'
                                     || IGT.groupedtags
               END AS export_galleries
FROM         Images I
    INNER JOIN      Albums A ON A.id = I.album
    INNER JOIN      AlbumRoots AR ON AR.id = A.albumRoot AND AR.id = 2 -- Export metadata only from a specific album root
    INNER JOIN      ImageGalleryTags IGT ON IGT.imageid = I.id
    LEFT OUTER JOIN ImageInformation II ON II.imageid = I.id
    LEFT OUTER JOIN ImagePositions IP ON IP.imageid = I.id
    LEFT OUTER JOIN ImagePublicTags IPT ON IPT.imageid = I.id
    LEFT OUTER JOIN ImageTitle ITIT ON ITIT.imageid = I.id
    LEFT OUTER JOIN ImageCaption ICAP ON ICAP.imageid = I.id
