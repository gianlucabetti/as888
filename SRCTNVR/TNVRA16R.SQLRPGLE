000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090423     H* TNVRA16R *---------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800090109     Ffifgt00f  uF a E             DISK    rename(FIFGT000:FGTFisico)
000900090424     F*****************************        usropn
001000090109     Ffitgt00f  o  a E           K DISK
001100090109      *
001200090423     Ffifpt01L  uF a E           K DISK
001300090423     Ffifpd01L  uF a E           K DISK
001400081211      *
001500090401     Ffiapd01L  iF   E           K DISK
001600090428     Ffiftt02L  iF   E           K DISK
001700090401      *
001800090424     FwxFGT00S  o  a E             DISK
001900081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000081211      * -------------------------------------------------------------
002100090421     d dapdflr       e ds
002200090421     dficn08ds       e ds
002300081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002400081110     d                                     prefix(STP:3)
002500081110     D esito           s             10i 0
002600081110     D FGT_REC         s              9s 0
002700090424     D STP_DATSCAD     s              8s 0
002800081230      *
002900090109     D KPJBA         E DS
003000090109      *
003100090109     D ficn05ds      E DS
003200090109     D dGPI          E DS
003300900521     C*****************************************************************
003400090112     C****+ dec( year(curdate())*10000 + month(curdate())*100 +
003500090112     C****+ day(curdate()) , 8, 0)
003600090112     C****+ between fgtddt and fgtdst
003700090112     C*****************************************************************
003800090422     C*
003900090423     c                   clear                   savSTPSMLpdr      8
004000081209     C                   SETON                                        LR
004100090422     C*
004200090422     C*=================================================================
004300020418     C*
004400090422     C*=================================================================
004500090422      **
004600090423      **  Ricordiamo che:
004700090423      **    PRIMA sono state fatte scadere al 30/04/2009 tutte le altre
004800090423      *      che non devono essere CONVERTITE.
004900090422      **
005000090423      **  Si divide in 2 il problema:
005100090423      **  1)Gli autisti che hanno tariffe relative ad un solo Periodo da ricondurre
005200090423      **    ad un unico progressivo 200 decorrenza 1/5 scadenza 31/12/2039
005300090423      **    e
005400090423      **  2)Gli autisti che invece hanno + tariffe in periodi diversi occorre generare
005500090423      **    più periodi consecutivi con progressivi 200, 201, ... sulla supertestata
005600090423      **    ed il 200 decorrerà dal 1/5 scedendo il giorno prima del giorno di
005700090423      **    decorrenza del progressivo 201 e così via......
005800090423      **
005900090423      **  ATTENZIONE: l'unica discriminante e se la tariffa era stata STAMPATA
006000090423      **   Se era stata STAMPATA sul nuovo progressivo le tariffe
006100090423      **   devono risultare
006200090423      **   CONFERMATE
006300090423      **
006400090423      **   Se NON era stata STAMPATA sul nuovo progressivo le tariffe
006500090423      **   devono risultare
006600090423      **   NON CONFERMATE
006700090423      **
006800090423     C*=================================================================
006900090423      **  Queste sono le tariffe da CONVERTIRE creando le SUPERTESTATE
007000090423     C*=================================================================
007100090423      ** 2° PROBLEMA   riconducibile al 1° PROBLEMA:
007200090423      **    DEVE leggere tutte le tariffe AUTISTI (C,R,G) con data decorrenza
007300090422      **  minore del 2010 e con Data SCADENZA superiore al 1°maggio 2009
007400090423      **  ma riconducibili a più periodi però con decorreza inferiore su tutte
007500090423      **  le tariffe al 1/5/2009 quindi il problema si riconduce al 1° problema
007600090423      **   ossia si devono far scadere tutte al 30/4/2009 e riaprire con il prog.200
007700090423      **   il 1/5/2009 facendo attenzione alla data di scadenza
007800090423      **  se almeno una delle tante scade entro il 2009 tutte
007900090423      **  scadranno alla stessa data.
008000090423      **
008100090424      *
008200090424      *  esempio:
008300090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA    DATAMIN
008400090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.   SCADENZA
008500090424      *                         PADR                          TAR.         TAR.
008600090424      *
008700090424      *     50285                 C    300    4    EUR    2009/04/21  2039/12/31  2009/09/30
008800090424      *     50285                 C    999         EUR    2009/01/26  2009/09/30  2009/09/30
008900090424      *     50285                 R    300    4    EUR    2009/04/21  2039/12/31  2009/09/30
009000090424      *
009100090424      *     50312                 C    300    1    EUR    2009/04/21  2039/12/31  2009/09/01
009200090424      *     50312                 C    999    1    EUR    2009/02/10  2009/09/01  2009/09/01
009300090424      *     50312                 R    300    1    EUR    2009/04/21  2039/12/31  2009/09/01
009400090424      *                                                       <>           <>      è la MIN
009500090424      *                                                      N O N  sono uguali    nell'AUT
009600090424      *
009700090423      *
009800090422     C/EXEC SQL DECLARE A1 CURSOR FOR
009900090424     C+   with AUTSIMULA (AUTISTA, SIMULA, DATAMAX) as (
010000090424     C+   select fgtpdr, fgtsml, max(fgtddt) from
010100090424     C+   fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
010200090424     C+   and fgtddt < 20100101 and fgtdst >= 20090501
010300090424     C+   group BY FGTpdr, FGTsml
010400090424     C+   having max(fgtddt) < 20090501)
010500090424     C+ SELECT DATAMIN, a.* , rrn(a) FROM
010600090424     C+ fifgt00f as a join AUTSIMULA on a.fgtpdr=Autista
010700090424     C+ and a.fgtsml=SIMULA
010800090424     C+ join (
010900090424     C+ select fgtpdr, fgtsml, min(fgtdst) as DATAMIN
011000090424     C+  from
011100090424     C+ fifgt00f WHERE  fgttsr in ('C', 'R', 'G')
011200090424     C+ and fgtddt < 20100101 and
011300090424     C+ fgtdst >= 20090501
011400090424     C+ group BY FGTpdr, FGTsml)  as SCADENZA
011500090424     C+  on a.fgtpdr=SCADENZA.fgtpdr and a.fgtsml=SCADENZA.fgtsml
011600090424     C+ WHERE  fgttsr in ('C', 'R', 'G')
011700090424     C+ and fgtddt < 20100101 and fgtdst >= 20090501
011800090424     C+ and a.fgtsml concat digits(a.fgtpdr) in
011900090424     C+ (
012000090424     C+ select fgtsml concat digits(fgtpdr) from
012100090424     C+ (
012200090424     C+ select FGTSML, FGTPDR, count(*) from
012300090424     C+ (
012400090424     C+ SELECT FGTSML, FGTPDR, FGTDDT, FGTDST  from
012500090424     C+ fifgt00f as a WHERE  fgttsr in ('C', 'R', 'G')
012600090424     C+ and fgtddt < 20100101 and fgtdst >= 20090501
012700090424     C+ group BY FGTSML, FGTPDR, FGTDDT, FGTDST
012800090424     C+ ) as b
012900090424     C+ group BY FGTSML, FGTPDR
013000090424     C+ having count(*) > 1
013100090424     C+ ) as unPERIODO
013200090424     C+ )
013300090424     C+ ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
013400090422     C/END-EXEC
013500081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
013600081110     c                   if        sqlCODE < 0
013700081110     c                   eval      esito = Sqlcode
013800081110     c                   dump(a)
013900081110     c                   return
014000081110     c                   end
014100081110     C/EXEC SQL
014200081110     C+ OPEN A1
014300081110     C/END-EXEC
014400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
014500081110     c                   if        sqlCODE < 0
014600081110     c                   eval      esito = Sqlcode
014700081110     c                   dump(a)
014800081110     c                   return
014900081110     c                   else
015000081110     c                   except    testa
015100081110     c                   end
015200090109      * >>>>>>>
015300081209      * resto nel ciclo finchè....
015400081110     C                   DOU       SqlCod < 0
015500081110      * lettura cursore
015600081110     C/EXEC SQL
015700090424     C+ Fetch Next From A1 Into :STP_DatSCAD, :dsFGT, :FGT_REC
015800081110     C/END-EXEC
015900081110      *  EoF
016000081110     C                   if        SqlCod = 100
016100081110     c                   Leave
016200081110      *  x errori
016300081110     C                   ElseIf    SqlCod < 0
016400081110     c                   eval      esito = Sqlcode
016500081110     c                   dump(a)
016600081110     c                   leave
016700081110     C                   ENDif
016800090428      *
016900090428      *----------------------------->>>>>
017000090428      * Controlli su Angrafica e su Valorizzazioni
017100090428      *
017200090428     c                   clear                   errore           10
017300090428      *
017400090428      * rileva dall'anagrafica
017500090428     c                   eval      apdTIP = 'A'
017600090428     c                   eval      apdPDR = stpPDR
017700090428     c     Key_APD       chain     fiapd01l
017800090428     c                   if        %Found(fiapd01l)
017900090428      *
018000090428     c                   if        apdATB <> *blank
018100090428     c                   movel     'ANNULLATO'   errore
018200090428     c                   else
018300090428      *
018400090428      *-  Esluso da autofatturazione
018500090428     c                   if        apdPDD = 'S'
018600090428     c                   movel     'AUTOFAT'     errore
018700090428     c                   end
018800090428      *
018900090428     c                   endIF
019000090428      *
019100090428     c                   else
019200090428     c                   movel     'NO_APD'      errore
019300090428     c                   end
019400090428      *
019500090428      *  Controlla sulle valorizzazioni che non ce ne siano oppure
019600090428      *   siano precedenti al 20090101
019700090429      *  SEMPRE CHE non siano in decorrenza fra il 1/4 ed il 31/12
019800090430      *
019900090430     c                   z-add     0             datamaxima        8 0
020000090430     C/EXEC SQL
020100090430     C+ SELECT max(fgtddt) INTO :datamaxima FROM edpab/svFgt00F WHERE
020200090430     C+ fgtpdr = :STPpdr and fgtsml= :STPsml and FGTTSR in ('C', 'R', 'G')
020300090430     C/END-EXEC
020400090430     c                   if        datamaxima < 20090401
020500090429      *
020600090428     c                   if        errore = *blank
020700090428     c     stppdr        chain     fiftt02l
020800090428     c                   if        not %Found(fiftt02l)
020900090428     c                   movel     'NO_in_FTT'   errore
021000090428     c                   end
021100090428      *
021200090428     c                   if        errore = *blank
021300090428      *
021400090428     c                   eval      fttpdr = stpPDR
021500090428     c                   eval      fttfvl = 'C'
021600090428      *
021700090428     c     Key_FTT       setgt     fiftt02l
021800090428     c     Key_FTT       readpe    fiftt02l
021900090428      * Se inferiore al 2009
022000090428     c                   if        not %Eof(fiftt02l)
022100090428      *
022200090428     c                   if        fttddc < 20090101
022300090428     c                   movel     'FTT_<_2009'  errore
022400090428     c                   end
022500090428      *  SE EoF
022600090428     c                   else
022700090428     c                   movel     'FTT_no_C'    errore
022800090428     c                   end
022900090428     c
023000090428     c                   end
023100090428     c                   end
023200090429     c                   end
023300090428      *----------------------------->>>>>
023400090428     c                   movel     errore        error5            5
023500090429      * NONOSTANTE l'errore se la Decorrenza è > del 30 Aprile
023600090429      *   devo tradurli
023700090429     c                   if         stpDDT > 20090430 and errore <> *blank
023800090429     c                   move      '/X'          error5
023900090429     c                   move      ' /EXE'       errore
024000090429     c                   end
024100090429      *
024200090428      *  se sono qui ho letto un record correttamente
024300090428     c   Of              except    testa
024400090428     c                   except    detta
024500081110      *
024600090428      *
024700090428      *----------------------------->>>>>
024800090428      * x qualsiasi tipo di errore controllato prima non deve fare nulla
024900090428      *
025000090428     c                   if        Errore  = *blank
025100090429     c                              or  stpDDT > 20090430
025200090428      *----------------------------->>>>>
025300090428      *
025400090109     c                   clear                   eseguito          1
025500090109     c                   clear                   verifica          1
025600081110      *
025700090427      *
025800081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
025900090423     c                   movel     stpSML        stpSMLPDR         8
026000090109     c                   move      stpPDR        Autista           7 0
026100090423     c                   move      Autista       stpSMLPDR
026200090109      *
026300090109      *  Solo in aggiornamento
026400081110     c                   if        Aggiorna = 'S'
026500090424      *
026600090109      *  Deve scrivere la testata
026700090109      *    a rottura di simulazione/autista
026800090423      *     salva chiave di rottura
026900090423     c                   if        STPsmlPDR <> savSTPsmlPDR
027000090423     c                   eval      savSTPsmlPDR = STPsmlPDR
027100090423      *
027200090423      *  IMPOSTA la SUERTESTATA
027300090109     c                   clear                   fitgt000
027400090423     c                   z-add     STPpdr        tgtPDR
027500090423     c                   move      STPsml        tgtSML
027600090423     c                   z-add     200           tgtPRG
027700090423     c                   move      STPdiv        tgtDIV
027800090423      * data decorrenza/scadenza
027900090423     c                   z-add     20090501      tgtDDT
028000090423      *
028100090424     c                   if        stp_DatSCAD > 20091231
028200090423     c                   z-add     20391231      tgtDST
028300090423     c                   else
028400090424     c                   z-add     stp_DatSCAD   tgtDST
028500090423     c                   end
028600090423      *
028700090423     c                   z-add     *date         tgtDUV
028800090423     c                   moveL     'Convers'     tgtPRU
028900090401      *
029000090423      * rileva dall'anagrafica
029100090401     c                   eval      apdTIP = 'A'
029200090401     c                   eval      apdPDR = tgtPDR
029300090401     c     Key_APD       chain     fiapd01l
029400090423      *---
029500090401     c                   if        %Found(fiapd01l)
029600090423      *   società fornitore
029700090421     c                   movel     apdcsf        tgtsoc
029800090421     c                   movel     *all'0'       tgtcdf
029900090421     c                   move      apdksc        tgtcdf
030000090421     c                   movel     apdflr        dapdflr
030100090423      *  la massa in KG
030200090421     c                   eval      tgtpkl = §APDMCP
030300090423     c*  i tempi di carico e scarico
030400090421     c                   clear                   ficn08ds
030500090421     c                   eval      i08data= tgtddt
030600090421     c                   eval      i08tip= 'A'
030700090421     c                   eval      i08mcp = tgtpkl
030800090421     c                   call      'FICN08R'
030900090421     c                   parm                    ficn08ds
031000090421     c                   if        o08err =  ' '
031100090421     c                   eval      TGTTMPC = o08tc
031200090421     c                   eval      TGTTMPS = o08ts
031300090421     c                   end
031400090423      *---
031500090401     c                   end
031600090423      *
031700090423      *  prezzo carburante al litro
031800090109     c                   z-add     D§GPIGPI      tgtPCAR
031900090422      * Attenzione:
032000090423      *     se non sono state stampate nel vecchio
032100090423      *        non devono essere convalidate nel nuovo
032200090422     c                   if        stpDTS > 0
032300090424     c                   z-add     20090501      tgtDTS
032400090422     c                   end
032500090422      *
032600090109     c                   write     fitgt000
032700090109     c                   end
032800090423      *------------------
032900081216      *
033000090423      *  genera o aggiorna
033100090423      *     i dettagli con il nuovo progressivo
033200090423     c                   exsr      FPT_FPD
033300090109      *
033400090109      *------------------
033500081218      *   deve eliminare i puntamenti del codice di riferimento
033600090424     c***********        open      fifgt00f
033700090423      *
033800081218     c     fgt_REC       chain     FIFGT00F
033900081218     c                   if        %Found(FIFGT00F)
034000090112      *
034100090422      *  Fa scadere la testata al 30 apr.2009
034200090423     c                   if            fgtddt < 20090501
034300090422     c                   eval      fgtDST = 20090430
034400090423     c                   end
034500090424      **
034600090424      *   CON L'SQL di SELEZIONE NON ESISTERA' MAI QUESTA SITUAZIONE
034700090424      **   poichè è in JOIN con la tabella temporanea x codice AUT
034800090424      **   con --> having max(fgtddt) < 20090501
034900090424      **
035000090422      *     Ma se decorreva già dal 1/5/2009
035100090422      *   non può farlo scadere il 30/4/2009
035200090112      * e quindi questa diventa la tariffa della supertestata
035300090423     c                   if            fgtddt = 20090501
035400090423      * data scadenza
035500090423     c                   if           fgtDST > 20091231
035600090423     c                   z-add     20391231      fgtDST
035700090423     c                   endIf
035800090423      *
035900090112     c                   z-add     200           fgtPRG
036000090112     c                   z-add     *date         fgtDUV
036100090112     c                   clear                   fgtDCN
036200090423      *
036300090423      * solo se era vuoto anche prima
036400090423     c                   if            stpDTS > 0
036500090424     c                   z-add     20090501      fgtDTS
036600090423     c                   elseif        stpDTS = 0
036700090112     c                   clear                   fgtDTS
036800090423     c                   end
036900090112     c                   clear                   fgtFTR
037000090112     c                   clear                   fgtDTR
037100090423      *
037200090423     c                   end
037300090423      *
037400090423      * aggiorna
037500081218     c                   update    fgtFisico
037600090109      *
037700090423      *  Quindi scrive un'altra testata SOLO SE decorrenza < 1/5/2009
037800090423     c                   IF           fgtDDT < 20090501
037900090420     c                   eval      fgtDDT = 20090501
038000090423      *
038100090423     c                   if        stpDST > 20091231
038200090423     c                   z-add     20391231      fgtDST
038300090423     c                   else
038400090424     c                   z-add     stp_DATscad   fgtDST
038500090423     c                   end
038600090423      *
038700090109     c                   z-add     200           fgtPRG
038800090109     c                   z-add     *date         fgtDUV
038900090109     c                   clear                   fgtDCN
039000090424     c*****************  z-add     tgtDTS        fgtDTS
039100090109     c                   clear                   fgtFTR
039200090109     c                   clear                   fgtDTR
039300090109     c                   write     fgtFisico
039400090423     c                   endIF
039500090112      *
039600090423     c                   endIF
039700090112      *
039800090424     c************       close     fifgt00f
039900090109      *
040000090109     c                   endif
040100090109      *------------------
040200090428      *----------------------------->>>>>
040300090428     c                   end
040400090428      *----------------------------->>>>>
040500090428      *
040600090424      *  scrive il dettaglio su un file
040700090424     c                   exsr      wri_DETTA
040800081110      *
040900081110     C                   ENDDO
041000081110     C/EXEC SQL
041100081110     C+ Close A1
041200081110     C/END-EXEC
041300081110     C                   if        Esito < 0
041400081110     c                   return
041500081110     c                   endIF
041600081110     C*
041700081110     C     FINE          TAG
041800081110     c                   except    finest
041900081110     c                   return
042000081209     C*------------------------------------------------------------------------*
042100081209     C* *INZSR - OPERAZIONI INIZIALI
042200081209     C*------------------------------------------------------------------------*
042300081209     C     *INZSR        BEGSR
042400081209      *
042500081209     C     *ENTRY        PLIST
042600081209     C                   PARM                    AGGIORNA          1
042700081209     C*
042800090109      * duplica le tariffe FGT
042900081209     C     fgt01K        kLIST
043000081211     C                   kfld                    stpPDR
043100081211     C                   kfld                    stpSML
043200081211     C                   kfld                    stpTSR
043300081211     C                   kfld                    stpCTR
043400081211     C                   kfld                    stpPRG
043500090109      *
043600090109      * duplica le tariffe FPT e FPD
043700090109     c     Key_FPT       KLIST
043800090109     c                   kfld                    stpPDR
043900090109     c                   kfld                    stpSML
044000090109     c                   kfld                    stpTSR
044100090109     c                   kfld                    stpCTR
044200090109     c                   kfld                    stpPRG
044300081209     C*
044400090401     c     Key_APD       KLIST
044500090401     c                   kfld                    apdTIP
044600090401     c                   kfld                    apdPDR
044700090428     C*
044800090428     c     Key_FTT       KLIST
044900090428     c                   kfld                    FTTPDR
045000090428     c                   kfld                    FTTFVL
045100090401     C*
045200081209     c                   clear                   esito
045300081209      *
045400081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
045500081209     C                   clear                   UDATE_8           8 0
045600081209     C                   MOVEL     *DATE         UDATE_8
045700090109      *
045800090109      * reperisce il prezzo di riferimento del carburante
045900090109     c                   call      'FICN05R'
046000090109     c                   parm                    ficn05DS
046100090109      *
046200090109     c                   iF        o05err = *blank
046300090109     c                   eval      dGPI = o05GPI
046400090109     c                   enD
046500090109      *
046600081209     c                   endSR
046700090109      *---------------------------------------------------------------*
046800090109      *  Scrive Testate e Dettaglio
046900090109      *---------------------------------------------------------------*
047000090423     c     FPT_FPD       begSR
047100090109     C*
047200090109     c     Key_FPT       setll     fifpt01L
047300090109     c     Key_FPT       reade     fifpt01L
047400090109     c                   Dow       not %EoF(fifpt01l)
047500090423     C*
047600090109     c                   z-add     200           fptPRG
047700090423     C*
047800090424     C* IN QUESTO CASO CI SARANNO SOLO LE WRITE poichè SQL prende
047900090424     C*  TUTTE LE DECORRENZE INFERIORI AL 1/5
048000090423      * se decorre dal 1/5 allora deve aggiornare
048100090423     c                   if        stpDDT = 20090501
048200090423     c                   update    fifpt000
048300090423     c                   else
048400090109     c                   write     fifpt000
048500090423     c                   end
048600090423      *
048700090109     c     Key_FPT       reade     fifpt01L
048800090109     c                   endDo
048900090109     C*
049000090109     c     Key_FPT       setll     fifpd01L
049100090109     c     Key_FPT       reade     fifpd01L
049200090109     c                   Dow       not %EoF(fifpd01l)
049300090423      *
049400090109     c                   z-add     200           fpdPRG
049500090423      *
049600090424     C* IN QUESTO CASO CI SARANNO SOLO LE WRITE poichè SQL prende
049700090424     C*  TUTTE LE DECORRENZE INFERIORI AL 1/5
049800090423      * se decorre dal 1/5 allora deve aggiornare
049900090423     c                   if        stpDDT = 20090501
050000090423     c                   update    fifpd000
050100090423     c                   else
050200090423     c                   write     fifpd000
050300090423     c                   end
050400090423      *
050500090109     c     Key_FPT       reade     fifpd01L
050600090109     c                   endDo
050700090109     C*
050800090109     c                   endSR
050900081211     C*---------------------------------------------------------------*
051000090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
051100090424      *---------------------------------------------------------------*
051200090424     c     WRI_DETTA     begSR
051300090424     C*
051400090424     c                   clear                   WSFGT000
051500090424     C*
051600090424     c                   eval      WSFSML  =  stpSML
051700090424     c                   eval      WSFPDR  =  stpPDR
051800090424     c                   eval      WSFTSR  =  stpTSR
051900090424     c                   eval      WSFCTR  =  stpCTR
052000090424     c                   eval      WSFPRG  = stpPRG
052100090424     c                   eval      WSFDDT  = stpDDT
052200090424     c                   eval      WSFDST  = stpDST
052300090424     c                   eval      WSFDTS  = stpDTS
052400090424     c                   eval      WSFDCN  = stpDCN
052500090424     c                   eval      WSFREC  = FGT_REC
052600090424     c                   eval      WSFDECOR= 0
052700090424     c                   eval      WSFSCAD = stp_DATscad
052800090424     c                   eval      WSFPROGR= 'TNVRA16R'
052900090424     c**
053000090428     c                   eval      WSFNOTE = Errore
053100090428     c**
053200090424     c                   write     WSFGT000
053300090424     C*
053400090424     c                   endSR
053500090424     C*---------------------------------------------------------------*
053600081110     OQSYSPRT   E            TESTA            02
053700081110     O                                           16 'Simul'
053800081110     O                                           25 'Autista'
053900081110     O                                           30 'TSR'
054000081110     O                                           38 'Tariffa'
054100081110     O                                           45 'Progr'
054200081110     O                                           55 'Data_Dec'
054300081110     O                                           65 'Data_Sca'
054400081110     O                                           75 'Data_Stp'
054500081110     O                                           85 'Data_Cnv'
054600081211     O                                          115 'N_Rec_FGT'
054700090424     O                                          125 'Data_Min'
054800081110     O          E            DETta       1
054900081110     O                       stpSML              14
055000081110     O                       stpPDR        z     25
055100081110     O                       stpTSR              29
055200081110     O                       stpCTR              36
055300081110     O                       stpPRG        z     43
055400081110     O                       stpDDT        z     55
055500081110     O                       stpDST        z     65
055600081110     O                       stpDTS        z     75
055700081110     O                       stpDCN        z     85
055800081211     o                       FGT_REC       z    115
055900090424     O                       stp_DATscad   z    125
056000090428     O                       ERROR5             132
056100081110      ****
056200081110     o          E            FINEST      2
056300090423     o                                           30 '*** TNVRA16R ***'
056400090102     o                                           59 '***   FINE  STAMPA  ***'
056500090109     o                                          + 9 '***  SUPER TARIFFA  ***'
