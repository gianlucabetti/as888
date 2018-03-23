000100180115     h dftactgrp(*no) actgrp(*caller) datedit(*ymd/)
000101180110      *-----------------
000102180110     Faitra03l  if   e           k disk
000200180110      //********************************************************************************************
000300180110      //
000400180110      // Definizione variabili.
000500180110      //
000600180110      //********************************************************************************************
000601180124     D cfiscale        S             16
000900180110     D Profilo         S             10
001200180110     D RTNesito        S             10I 0
001300180110     D RTNopcode       S             10
001400180110     D RTNstatus       S             10I 0
001500180110
001501180115     d oggi            s              8  0
001600180110     d wtime           s             17
001601180110     d cfisc           s             16
001602180110     d w               s              2  0
001603180111     d codice9         s              9
001604180110     d codaut          s              7  0
001605180110     d codice          s              7  0
001606180110     d codici          ds                   occurs(50)
001607180111     d  skcod                         9     inz
001700180110      //********************************************************************************************
001800180110      //
001900180110      // Definizione parametri procedura.
002000180110      //
002100180110      //********************************************************************************************
002200180110     C     *ENTRY        PLIST
002201180124     C                   PARM                    Cfiscale
002300180110     C                   PARM                    Profilo
002600180110     C                   PARM                    rtnesito
002700180110     C                   PARM                    rtnopcode
002800180110     C                   PARM                    rtnstatus
002900180110
003000180110      /FREE
003100180110
003200180110       //*******************************************************************************************
003300180110       //
003400180110       // Main.
003500180110       //
003600180110       //*******************************************************************************************
003700180110       CLEAR rtnesito;
003800180110       CLEAR rtnopcode;
003900180110       CLEAR rtnstatus;
004000180110       *INLR = *ON;
004100180110
004200180110       // imposto la data e ora del lavoro
004300180110       wtime = %char(%timestamp:*iso0);
004400180110
004500180110      /END-FREE
005300180110      *____________________________________________________________
005301180110     c*
005302180115     c                   move      *date         oggi
005303180124     c                   z-add     0             w
005304180124     c                   clear                   codici
005305180124      * verifico se ricevuto già codice fiscale oppure codice autista
005306180124     c                   if        Cfiscale <> *blank
005307180124     c                   movel     cfiscale      cfisc
005308180124     c                   else
005309180111     c                   movel     profilo       codice9
005310180111     c                   move      codice9       codaut
005311180124      * imposta se stesso sempre NON PIÙ (forzatura sospesa per ora)
005312180116     c                   z-add     0             w
005313180116     c*m                 z-add     1             w
005314180116     c*m   W             occur     codici
005315180116     c*m                 movel     'AU'          skcod
005316180116     c*m                 move      codaut        skcod
005317180111      *
005318180110     c     codaut        setll     aitra03l
005319180110     c                   do        *hival
005320180110     c     codaut        reade     aitra03l
005321180110     c                   if        %eof(aitra03l)
005322180111     c                   leave
005323180110     c                   endif
005324180110      * non controllo accreditamento valido ma se presente C.F.
005325180110      * nel caso in caso di disaccreditamento non venisse aggiornato il record sul fiapd4
005326180110      * bisogna controllare con la specifica asteriscata e non con il solo C.F
005327180110     c                   if        tracf <> *blank
005328180110     c                   movel     tracf         cfisc
005329180110     c                   leave
005330180110     c                   endif
005331180110     c                   enddo
005332180124      *
005333180124     c                   endif
005334180116     c*
005335180124     c                   if        cfisc <> *blank
005336180110     c*
005337180110     C/EXEC SQL
005338180110     C+ DECLARE AUT CURSOR FOR SELECT trakau
005339180115     C+ FROM aitra00f WHERE tracf = :cfisc and traann = ' ' and
005340180201     C+ (tradfi = 0 or tradfi > :oggi) and trakau > 0
005341180110     C+ Group by trakau
005342180110     C/END-EXEC
006900180110     C
006901180110     C/EXEC SQL
006902180110     C+ OPEN AUT
006903180110     C/END-EXEC
006904180110     C                   DOU       SqlCod <> 0
006905180110     C/EXEC SQL
006906180110     C+ FETCH NEXT FROM AUT INTO :codice
006907180110     C/END-EXEC
006908180110     C                   SELECT
006909180110     c*
006910180110     C                   WHEN      SqlCod = 0
006911180116     c*m                 if        codaut <> codice
006912180110     c                   add       1             w
006913180110     c     W             occur     codici
006914180111     c                   movel     'AU'          skcod
006915180111     c                   move      codice        skcod
006917180116     c*m                 endif
006926180110     C*
006927180110      *   EoF
006928180110     C                   WHEN      SqlCod = 100
006929180110     c                   leave
006930180110      **
006931180110      *   Errori
006932180110     C                   WHEN      SqlCod <> 0
006933180110     c                   leave
006934180110      **
006935180110     C                   ENDSL
006936180110      **
006937180110     C                   ENDDO
006938180110     c                   endif
006939180110     C/EXEC SQL
006940180110     C+ CLOSE AUT
006941180110     C/END-EXEC
006942180110      *
007300180110
017700180110      /FREE
017800180110
018601180110        if w > 0;
018602180110
018603180110           exec sql   set result sets
018604180110                      for return to client
018605180110                      array :codici for :W rows;
018606180110            else;
018607180110                   rtnopcode='Non trovato';
018608180110                   rtnstatus = -1;
018609180110                   rtnesito = -1;
018610180110         endif;
018700180110
018800180110       RETURN;
018900180110
019000180110      /END-FREE
