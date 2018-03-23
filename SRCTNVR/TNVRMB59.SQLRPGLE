000100031030     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200071019     h dftactgrp(*no) actgrp(*caller)
000300031017
000400071018      *------------------------------------------------------------------------*
000500071018      *                                                                        *
000600071018      *       Sistema dati per GEO ORM                                         *
000700071018      *                                                                        *
000800071018      *------------------------------------------------------------------------*
000900071018     ffnorm01l  uf   e           k disk
001000071018     ffnorg01l  uf   e           k disk
001100070308
001200070308      *------------------------------------------------------------------------*
001300070926      *?D S   I N T E R N E / E S T E R N E
001400071011     d fnorgds       e ds                  extname(fnorg00f)
001500070927     d fnormds       e ds                  extname(fnorm00f)
001600070926
001700070308      *------------------------------------------------------------------------*
001800071018
001900080225     C/EXEC SQL
002000080225     C+ DECLARE B1 CURSOR FOR SELECT fnorm01l.*, fnorg01l.*
002100080225     C+ FROM fnorm01l join fnorg01l on
002200080225     C+ ormpoe=orgpoe and ormnsr=orgnsr and ormnor=orgnor and
002300080225     C+ ormnrv=orgnrv
002400170719     C+ WHERE ormpor = 123 and
002500090810     C+ ormfao < 600 and ormnsr = 0
002600080225     C/END-EXEC
002700080225
002800080225     C/EXEC SQL
002900080225     C+ OPEN B1
003000080225     C/END-EXEC
003100080225
003200080225     c                   do        *hival
003300080225
003400080225     C/EXEC SQL
003500080225     C+ FETCH NEXT FROM B1 INTO :fnormds, :fnorgds
003600080225     C/END-EXEC
003700080225
003800080225     c                   if        sqlcod = 100 or
003900080225     c                             sqlcod < 0
004000080225     c                   leave
004100080225     c                   endif
004200080225
004300170719     c                   IF        ormdao < 20170718
004400080225     c                   exsr      sr_sistema
004500120110     c                   ENDIF
004600080225     c                   enddo
004700080225
004800080225     C/EXEC SQL
004900080225     C+ CLOSE B1
005000080225     C/END-EXEC
005100070927
005200070308     c                   eval      *inlr = *on
005300070308
005400070308      *------------------------------------------------------------------------*
005500080225      *? sistemo i dati degli ORM
005600070308      *------------------------------------------------------------------------*
005700071018     c     sr_sistema    begsr
005800071018
005900080225     c     korm          chain     fnorg01l
006000080225     c                   if        %found(fnorg01l)
006100090810      * modifico la filiale ritiro su ORG
006200170719     c                   eval      orgpor = 072
006300090810      * cancello eventuale giro impostato
006400090810     c                   clear                   orgtgi
006500090810     c                   clear                   orgpocgi
006600090810     c                   clear                   orgcgi
006700090810     c                   clear                   orgdtvdis
006800090810     c                   clear                   orghvdis
006900080225     c                   update    fnorg000
007000080225     c                   endif
007100080225
007200090810      * modifico la filiale ritiro su ORM
007300080225     c     korm          chain     fnorm01l
007400080225     c                   if        %found(fnorm01l)
007500170719     c                   eval      ormpor = 072
007600080225     c                   update    fnorm000
007700080225     c                   endif
007800071018
007900071018     c                   endsr
008000070926
008100070926      *------------------------------------------------------------------------*
008200071018      *?ROUTINE INIZIALE
008300070926      *------------------------------------------------------------------------*
008400070926     c     *inzsr        begsr
008500071018
008600071018     c     korm          klist
008700071018     c                   kfld                    ormpoe
008800071018     c                   kfld                    ormnsr
008900071018     c                   kfld                    ormnor
009000071018     c                   kfld                    ormnrv
009100070926
009200070926     c                   endsr
