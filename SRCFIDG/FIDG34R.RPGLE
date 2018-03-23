000100070413     H DECEDIT('0,') DATEDIT(*YMD.)
000200070416     H*
000300070416     FFIARG01L  IF   E           K DISK
000400070406     D*
000500070416     D KPJBA         E DS
000600070416     D FIDG09DS      E DS
000700070416     D FIDG34DS      E DS
000800070416     D*
000900070416     D wPOC            S              3S 0 inz
001000070416     D wDAT            S              8S 0 inz
001100050103     C**********************************************************************
001200040917     C     *ENTRY        PLIST
001300070416     C                   PARM                    FIDG34DS
001400070416     C*
001500070416     C* Inizializzo buffer d output
001600070416     C                   EXSR      INZOUT
001700070420     C*
001800070420     C* Verifico tipo elaborazione richiesta
001900070420     C                   SELECT
002000070420     C                   WHEN      iD34tla = *blanks OR
002100070420     C                             iD34tla = 'E'
002200070420     C*
002300070420     C* Verifico tipo lancio richiesto
002400070420     C                   SELECT
002500070420     C                   WHEN      iD34tip = '1'
002600070420     C*
002700070420     C* Se passati anche parametri opzionali gestisco reperimento info relative
002800070420     C                   IF        iD34poc <> *zeros AND
002900070420     C                             iD34dri <> *zeros AND
003000070420     C                             iD34cgi <> *blanks
003100070420     C*
003200070420     C* Overrido i valori del fiarg con quelli passati in input
003300070420     C                   EVAL      argPOC    = iD34poc
003400070420     C                   EVAL      argDTVTEL = iD34dri
003500070420     C                   EVAL      argCGI    = iD34cgi
003600070420     C*
003700070420     C* Reperimento informazioni anagrafico giri tramite driver FIDG09R
003800070420     C                   IF        oD34esito = *blanks
003900070420     C                   EXSR      EXEGRG
004000070420     C                   ENDIF
004100070420     C*
004200070420     C* Reperimento informazioni versioni giri consegna da FIGRV
004300070420     C                   IF        oD34esito = *blanks
004400070420     C                   EXSR      EXEGRV
004500070420     C                   ENDIF
004600070420     C                   ENDIF
004700070420     C*
004800070420     C* Valorizzazione buffer d output
004900070420     C                   IF        oD34esito = *blanks
005000070420     C                   EXSR      OUTINF1
005100070420     C                   ENDIF
005200070420     C*
005300070420     C                   WHEN      iD34tip = '2'
005400070416     C*
005500070416     C* Reperimento informazioni da FIARG
005600070416     C                   IF        oD34esito = *blanks
005700070416     C                   EXSR      EXEARG
005800070416     C                   ENDIF
005900070416     C*
006000070416     C* Reperimento informazioni anagrafico giri tramite driver FIDG09R
006100070416     C                   IF        oD34esito = *blanks
006200070416     C                   EXSR      EXEGRG
006300070416     C                   ENDIF
006400070416     C*
006500070416     C* Reperimento informazioni versioni giri consegna da FIGRV
006600070416     C                   IF        oD34esito = *blanks
006700070416     C                   EXSR      EXEGRV
006800070416     C                   ENDIF
006900070416     C*
007000070416     C* Valorizzazione buffer d output
007100070416     C                   IF        oD34esito = *blanks
007200070420     C                   EXSR      OUTINF2
007300070416     C                   ENDIF
007400070420     C                   ENDSL
007500070420     C                   ENDSL
007600070420     C*
007700070420     C* Verifico tipo elaborazione richiesta
007800070420     C                   SELECT
007900070420     C                   WHEN      iD34tla = *blanks
008000070420     C                   SETON                                        LR
008100070420     C*
008200070420     C                   WHEN      iD34tla = 'C'
008300070420     C                   SETON                                        LR
008400070420     C*
008500070420     C                   WHEN      iD34tla = 'E'
008600070420     C                   SETON                                        RT
008700070420     C                   ENDSL
008800951025     C*
008900070416     C*--------------------------------------------------------------------------------------------*
009000070416     C* INZOUT - Inizializzazione buffer d output
009100070416     C*--------------------------------------------------------------------------------------------*
009200070416     C     INZOUT        BEGSR
009300070416     C*
009400070420     C                   clear                   oD34ttl
009500070420     C                   clear                   oD34ttldes
009600070420     C                   clear                   oD34tgi
009700070420     C                   clear                   oD34tgides
009800070420     C                   clear                   oD34cgi
009900070420     C                   clear                   oD34cgides
010000070420     C                   clear                   oD34ver
010100070420     C                   clear                   oD34verdes
010200070420     C                   clear                   oD34idb
010300070420     C                   clear                   oD34sco
010400070420     C                   clear                   oD34datela
010500070420     C                   clear                   oD34oraela
010600070420     C                   clear                   oD34lno
010700070420     C                   clear                   oD34lnodes
010800070420     C                   clear                   oD34err
010900070420     C                   clear                   oD34errdes
011000070420     C                   clear                   oD34sts
011100070420     C                   clear                   oD34stsdes
011200070420     C                   clear                   oD34esito
011300070420     C*
011400070420     C                   clear                   fidg09ds
011500070420     C                   clear                   Out_KeyVer
011600070420     C                   clear                   Out_VerDes
011700070416     C*
011800070416     C                   ENDSR
011900070416     C*--------------------------------------------------------------------------------------------*
012000070416     C* EXEARG - Reperimento informazioni da FIARG
012100070416     C*--------------------------------------------------------------------------------------------*
012200070416     C     EXEARG        BEGSR
012300070416     C*
012400070416     C     KEYarg01_c    chain     fiarg01l
012500070416     C                   if        %found(fiarg01l)
012600070416     C                   else
012700070416     C                   eval      oD34esito = '1'
012800070416     C                   endif
012900070416     C*
013000070416     C                   ENDSR
013100070416     C*--------------------------------------------------------------------------------------------*
013200070416     C* EXEGRG - Reperimento informazioni anagrafico giri tramite driver FIDG09R
013300070416     C*--------------------------------------------------------------------------------------------*
013400070416     C     EXEGRG        BEGSR
013500070416     C*
013600070416     C                   clear                   fidg09ds
013700070416     C                   eval      d09iop0 = '001'
013800070416     C                   eval      d09ifgs = argPOC
013900070416     C                   eval      d09idat = argDTVTEL
014000070416     C                   eval      d09icgi = argCGI
014100070706     C                   eval      d09itug = 'C'
014200070416     C                   eval      kpjbu = fidg09ds
014300070416     C                   call(e)   'FIDG09R'
014400070416     C                   parm                    kpjba
014500070416     C                   eval      fidg09ds = kpjbu
014600070416     C*
014700070416     C                   ENDSR
014800070416     C*--------------------------------------------------------------------------------------------*
014900070416     C* EXEGRV - Reperimento informazioni versioni giri consegna da FIGRV
015000070416     C*--------------------------------------------------------------------------------------------*
015100070416     C     EXEGRV        BEGSR
015200070416     C*
015300070416     C* Reperisco versioni giri x filiale/data richieste
015400070416     C                   call(e)   'FIDG11R1'
015500070416     C                   parm      argPOC        wPOC
015600070416     C                   parm      argDTVTEL     wDAT
015700070416     C                   parm      *blanks       Out_KeyVer        8
015800070416     C                   parm      *blanks       Out_VerDes       30
015900070416     C                   parm      *blanks       Out_Esito         1
016000070416     C*
016100070416     C                   ENDSR
016200070420     C*--------------------------------------------------------------------------------------------*
016300070420     C* OUTINF1 - Valorizzazione buffer d output x tipo lancio "1"
016400070420     C*--------------------------------------------------------------------------------------------*
016500070420     C     OUTINF1       BEGSR
016600070420     C*
016700070420     C                   eval      oD34cgi    = d09ocgi
016800070420     C                   eval      oD34cgides = d09odes
016900070420     C*
017000070420     C                   eval      oD34ver    = Out_KeyVer
017100070420     C                   eval      oD34verdes = Out_VerDes
017200070420     C*
017300070420     C                   eval      oD34lno    = iD34lno
017400070420     C                   select
017500070420     C                   when      oD34lno = 'V'
017600070420     C                   eval      oD34lnodes = 'LIVELLO VIA'
017700070420     C                   when      oD34lno = 'L'
017800070420     C                   eval      oD34lnodes = 'LIVELLO LOCALITA'
017900070420     C                   when      oD34lno = 'C'
018000070420     C                   eval      oD34lnodes = 'LIVELLO COMUNE'
018100070420     C                   when      oD34lno = 'P'
018200070420     C                   eval      oD34lnodes = 'LIVELLO PROVINCIA'
018300070420     C                   when      oD34lno = 'VA'
018400070420     C                   eval      oD34lnodes = 'VIA AMBIGUA'
018500070420     C                   when      oD34lno = 'LA'
018600070420     C                   eval      oD34lnodes = 'LOCALITA AMBIGUA'
018700070420     C                   when      oD34lno = 'CA'
018800070420     C                   eval      oD34lnodes = 'COMUNE AMBIGUO'
018900070420     C                   when      oD34lno = 'PA'
019000070420     C                   eval      oD34lnodes = 'PROVINCIA AMBIGUA'
019100070420     C                   when      oD34lno = 'N'
019200070420     C                   eval      oD34lnodes = 'MANCA CARTOGRAFIA'
019300070420     C                   other
019400070420     C                   eval      oD34lnodes = *all'*'
019500070420     C                   endsl
019600070420     C*
019700070420     C                   eval      oD34err    = iD34err
019800070420     C                   select
019900070420     C                   when      oD34err = '01'
020000070424     C                   eval      oD34errdes = 'ZONA GEO NON COPERTA DA GIRO'
020100070420     C                   when      oD34err = '02'
020200070420     C                   eval      oD34errdes = 'FILIALE GEO NON ATTIVA'
020300070420     C                   when      oD34err = '03'
020400070420     C                   eval      oD34errdes = 'SPEDIZIONE NON OTTIMIZZATA'
020500070420     C                   when      oD34err = '04'
020600070420     C                   eval      oD34errdes = 'PROFILO GEO NON ATTIVO'
020700070420     C                   when      oD34err = '05'
020800070420     C                   eval      oD34errdes = 'VERSIONE GEO GIRI NON TROVATA'
020900070420     C                   when      oD34err = '10'
021000070420     C                   eval      oD34errdes = 'GIRO REPERITO'
021100070420     C                   when      oD34err = '11'
021200070420     C                   eval      oD34errdes = 'GIRO REPERITO'
021300070420     C                   when      oD34err = '12'
021400070420     C                   eval      oD34errdes = 'GIRO REPERITO'
021500070420     C                   when      oD34err = '51'
021600070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN NORMALIZZAZ.'
021700070420     C                   when      oD34err = '52'
021800070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN OTTIMIZZAZ.'
021900070420     C                   when      oD34err = *blanks and iD34cgi = *blanks
022000070420     C                   eval      oD34errdes = 'GIRO NON REPERITO'
022100070420     C                   other
022200070420     C                   eval      oD34errdes = *blanks
022300070420     C                   endsl
022400070420     C*
022500070420     C                   eval      oD34sts    = *all'*'
022600070420     C                   eval      oD34stsdes = 'DATI INCONGRUENTI O MANUALI '
022700070420     C                   select
022800070420     C                   when      (iD34lno =  'V'   or
022900070420     C                              iD34lno =  'VA') and
023000070420     C                              iD34cgi <> *blanks
023100070420     C                   eval      oD34sts    = 'OK'
023200070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO VIA'
023300070420     C                   when      (iD34lno =  'L'   or
023400070420     C                              iD34lno =  'LA') and
023500070420     C                              iD34cgi <> *blanks
023600070420     C                   eval      oD34sts    = 'OK'
023700070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO LOCALITA'
023800070420     C                   when      (iD34lno =  'C'   or
023900070420     C                              iD34lno =  'CA') and
024000070420     C                              iD34cgi <> *blanks
024100070420     C                   eval      oD34sts    = 'OK'
024200070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO COMUNE'
024300070420     C                   when      (iD34lno =  'P'   or
024400070420     C                              iD34lno =  'PA') and
024500070420     C                              iD34cgi <> *blanks
024600070420     C                   eval      oD34sts    = 'OK'
024700070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO PROVINCIA'
024800070420     C                   when      (iD34err = '02'  or
024900070420     C                              iD34err = '04') and
025000070420     C                              iD34cgi = *blanks
025100070420     C                   eval      oD34sts    = 'KO'
025200070420     C                   eval      oD34stsdes = 'ERRORE CONFIGURAZIONE GEO'
025300070420     C                   when      (iD34err = '51'  or
025400070420     C                              iD34err = '52') and
025500070420     C                              iD34cgi = *blanks
025600070420     C                   eval      oD34sts    = 'KO'
025700070420     C                   eval      oD34stsdes = 'ERRORE DI SISTEMA GEO'
025800070420     C                   when      iD34lno = 'N' and
025900070420     C                             iD34cgi = *blanks
026000070420     C                   eval      oD34sts    = 'KO'
026100070420     C                   eval      oD34stsdes = 'CARTOGRAFIA MANCANTE'
026200070420     C                   endsl
026300070420     C*
026400070420     C                   ENDSR
026500070416     C*--------------------------------------------------------------------------------------------*
026600070420     C* OUTINF2 - Valorizzazione buffer d output x tipo lancio "2"
026700070416     C*--------------------------------------------------------------------------------------------*
026800070420     C     OUTINF2       BEGSR
026900070416     C*
027000070416     C                   eval      oD34ttl    = argTTL
027100070416     C                   select
027200070416     C                   when      oD34ttl = '1'
027300070423     C                   eval      oD34ttldes = 'SI - CON GIRO'
027400070416     C                   when      oD34ttl = '2'
027500070423     C                   eval      oD34ttldes = 'SI - SENZA GIRO'
027600070416     C                   when      oD34ttl = *blanks
027700070423     C                   eval      oD34ttldes = 'NO ELAB. GEO'
027800070416     C                   endsl
027900070416     C*
028000070416     C                   eval      oD34tgi    = argTGI
028100070416     C                   select
028200070416     C                   when      oD34tgi = 'A'
028300070416     C                   eval      oD34tgides = 'AUTOMATICO'
028400070416     C                   when      oD34tgi = 'M'
028500070416     C                   eval      oD34tgides = 'MANUALE'
028600070416     C                   when      oD34tgi = *blanks
028700070416     C                   eval      oD34tgides = 'GESTIONE GIRO MANCANTE'
028800070416     C                   endsl
028900070416     C*
029000070416     C                   eval      oD34cgi    = argCGI
029100070416     C                   eval      oD34cgides = d09odes
029200070416     C*
029300070416     C                   eval      oD34ver    = Out_KeyVer
029400070416     C                   eval      oD34verdes = Out_VerDes
029500070416     C*
029600070416     C                   eval      oD34idb    = argIDB
029700070416     C                   eval      oD34sco    = argSCO
029800070416     C                   eval      oD34datela = argDTVTEL
029900070416     C                   eval      oD34oraela = argHVTEL
030000070416     C*
030100070416     C                   eval      oD34lno    = argLNO
030200070423     C***                select
030300070423     C***                when      oD34tgi = 'A'
030400070423     C                   select
030500070423     C                   when      oD34ttl <> *blanks
030600070416     C                   select
030700070416     C                   when      oD34lno = 'V'
030800070420     C                   eval      oD34lnodes = 'LIVELLO VIA'
030900070416     C                   when      oD34lno = 'L'
031000070420     C                   eval      oD34lnodes = 'LIVELLO LOCALITA'
031100070416     C                   when      oD34lno = 'C'
031200070420     C                   eval      oD34lnodes = 'LIVELLO COMUNE'
031300070416     C                   when      oD34lno = 'P'
031400070420     C                   eval      oD34lnodes = 'LIVELLO PROVINCIA'
031500070416     C                   when      oD34lno = 'VA'
031600070416     C                   eval      oD34lnodes = 'VIA AMBIGUA'
031700070416     C                   when      oD34lno = 'LA'
031800070416     C                   eval      oD34lnodes = 'LOCALITA AMBIGUA'
031900070416     C                   when      oD34lno = 'CA'
032000070416     C                   eval      oD34lnodes = 'COMUNE AMBIGUO'
032100070416     C                   when      oD34lno = 'PA'
032200070416     C                   eval      oD34lnodes = 'PROVINCIA AMBIGUA'
032300070416     C                   when      oD34lno = 'N'
032400070416     C                   eval      oD34lnodes = 'MANCA CARTOGRAFIA'
032500070416     C                   other
032600070423     C***                eval      oD34lnodes = *all'*'
032700070423     C                   eval      oD34lnodes = *blanks
032800070416     C                   endsl
032900070423     C                   endsl
033000070416     C*
033100070416     C                   eval      oD34err    = argERR
033200070423     C***                select
033300070423     C***                when      oD34tgi = 'A'
033400070423     C                   select
033500070423     C                   when      oD34ttl <> *blanks
033600070416     C                   select
033700070416     C                   when      oD34err = '01'
033800070424     C                   eval      oD34errdes = 'ZONA GEO NON COPERTA DA GIRO'
033900070416     C                   when      oD34err = '02'
034000070416     C                   eval      oD34errdes = 'FILIALE GEO NON ATTIVA'
034100070416     C                   when      oD34err = '03'
034200070416     C                   eval      oD34errdes = 'SPEDIZIONE NON OTTIMIZZATA'
034300070416     C                   when      oD34err = '04'
034400070416     C                   eval      oD34errdes = 'PROFILO GEO NON ATTIVO'
034500070420     C                   when      oD34err = '05'
034600070420     C                   eval      oD34errdes = 'VERSIONE GEO GIRI NON TROVATA'
034700070416     C                   when      oD34err = '10'
034800070416     C                   eval      oD34errdes = 'GIRO REPERITO'
034900070416     C                   when      oD34err = '11'
035000070416     C                   eval      oD34errdes = 'GIRO REPERITO'
035100070416     C                   when      oD34err = '12'
035200070416     C                   eval      oD34errdes = 'GIRO REPERITO'
035300070416     C                   when      oD34err = '51'
035400070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN NORMALIZZAZ.'
035500070416     C                   when      oD34err = '52'
035600070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN OTTIMIZZAZ.'
035700070424     C                   when      oD34err = *blanks and
035800070424     C                             (argCGI = *blanks or argTTL='2')
035900070416     C                   eval      oD34errdes = 'GIRO NON REPERITO'
036000070416     C                   other
036100070416     C                   eval      oD34errdes = *blanks
036200070423     C                   endsl
036300070510     C                   when      oD34tgi = 'M'     and oD34cgi <> *blanks
036400070510     C                   eval      oD34err    = *blanks
036500070510     C                   eval      oD34errdes = 'GIRO ATTRIBUITO MANUALMENTE'
036600070510     C                   when      oD34tgi = *blanks
036700070416     C                   eval      oD34err    = *blanks
036800070416     C                   eval      oD34errdes = 'GESTIONE GIRO MANCANTE'
036900070416     C                   endsl
037000070416     C*
037100070420     C                   eval      oD34sts    = *all'*'
037200070420     C                   eval      oD34stsdes = 'DATI INCONGRUENTI O MANUALI '
037300070416     C                   select
037400070416     C                   when      oD34tgi = 'A'
037500070416     C                   select
037600070416     C                   when      (argLNO =  'V'   or
037700070416     C                              argLNO =  'VA') and
037800070416     C                              argCGI <> *blanks
037900070416     C                   eval      oD34sts    = 'OK'
038000070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO VIA'
038100070416     C                   when      (argLNO =  'L'   or
038200070416     C                              argLNO =  'LA') and
038300070416     C                              argCGI <> *blanks
038400070416     C                   eval      oD34sts    = 'OK'
038500070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO LOCALITA'
038600070416     C                   when      (argLNO =  'C'   or
038700070416     C                              argLNO =  'CA') and
038800070416     C                              argCGI <> *blanks
038900070416     C                   eval      oD34sts    = 'OK'
039000070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO COMUNE'
039100070416     C                   when      (argLNO =  'P'   or
039200070416     C                              argLNO =  'PA') and
039300070416     C                              argCGI <> *blanks
039400070416     C                   eval      oD34sts    = 'OK'
039500070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO PROVINCIA'
039600070416     C                   when      (argERR = '02'  or
039700070416     C                              argERR = '04') and
039800070416     C                              argCGI = *blanks
039900070416     C                   eval      oD34sts    = 'KO'
040000070416     C                   eval      oD34stsdes = 'ERRORE CONFIGURAZIONE GEO'
040100070416     C                   when      (argERR = '51'  or
040200070416     C                              argERR = '52') and
040300070416     C                              argCGI = *blanks
040400070416     C                   eval      oD34sts    = 'KO'
040500070416     C                   eval      oD34stsdes = 'ERRORE DI SISTEMA GEO'
040600070416     C                   when      argLNO = 'N' and
040700070416     C                             argCGI = *blanks
040800070416     C                   eval      oD34sts    = 'KO'
040900070416     C                   eval      oD34stsdes = 'CARTOGRAFIA MANCANTE'
041000070416     C                   endsl
041100070416     C                   when      oD34tgi = 'M'
041200070416     C                   eval      oD34sts    = 'OK'
041300070424     C                   eval      oD34stsdes = 'GIRO ATTRIBUITO MANUALMENTE'
041400070416     C                   when      oD34tgi = *blanks
041500070416     C                   eval      oD34sts    = 'KO'
041600070416     C                   eval      oD34stsdes = 'GESTIONE GIRO MANCANTE'
041700070416     C                   endsl
041800070416     C*
041900070416     C                   ENDSR
042000070416     C*--------------------------------------------------------------------------------------------*
042100070416     C* *PSSR  - Gestione errori imprevisti
042200070416     C*--------------------------------------------------------------------------------------------*
042300070416     C     *PSSR         BEGSR
042400070416     C*
042500070416     C                   eval      oD34esito = '2'
042600070416     C*
042700070416     C                   seton                                        LR
042800070416     C*
042900070416     C                   ENDSR     '*CANCL'
043000070330     C*--------------------------------------------------------------------------------------------*
043100070330     C* *inzsr - operazioni iniziali
043200070330     C*--------------------------------------------------------------------------------------------*
043300070330     C     *inzsr        BEGSR
043400070330     C*
043500070416     C* Definizione chiavi
043600070416     C*
043700070416     C* Chiave su FIARG01L - Completa
043800070416     C     KEYarg01_c    KLIST
043900070416     C                   KFLD                    iD34aas
044000070416     C                   KFLD                    iD34lnp
044100070416     C                   KFLD                    iD34nrs
044200070416     C                   KFLD                    iD34nsp
044300070330     C*
044400070330     C                   ENDSR
