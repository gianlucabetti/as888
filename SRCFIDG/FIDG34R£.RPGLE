000100070413     H DECEDIT('0,') DATEDIT(*YMD.)
000101070416     H*
000102070416     FFIARG01L  IF   E           K DISK
002008070406     D*
002009070416     D KPJBA         E DS
002010070416     D FIDG09DS      E DS
002011070416     D FIDG34DS      E DS
002012070416     D*
002013070416     D wPOC            S              3S 0 inz
002014070416     D wDAT            S              8S 0 inz
002100050103     C**********************************************************************
002200040917     C     *ENTRY        PLIST
002300070416     C                   PARM                    FIDG34DS
002301070416     C*
002302070416     C* Inizializzo buffer d output
002303070416     C                   EXSR      INZOUT
002304070420     C*
002305070420     C* Verifico tipo elaborazione richiesta
002306070420     C                   SELECT
002307070420     C                   WHEN      iD34tla = *blanks OR
002308070420     C                             iD34tla = 'E'
002309070420     C*
002310070420     C* Verifico tipo lancio richiesto
002311070420     C                   SELECT
002312070420     C                   WHEN      iD34tip = '1'
002313070420     C*
002314070420     C* Se passati anche parametri opzionali gestisco reperimento info relative
002315070420     C                   IF        iD34poc <> *zeros AND
002316070420     C                             iD34dri <> *zeros AND
002317070420     C                             iD34cgi <> *blanks
002318070420     C*
002319070420     C* Overrido i valori del fiarg con quelli passati in input
002320070420     C                   EVAL      argPOC    = iD34poc
002321070420     C                   EVAL      argDTVTEL = iD34dri
002322070420     C                   EVAL      argCGI    = iD34cgi
002323070420     C*
002324070420     C* Reperimento informazioni anagrafico giri tramite driver FIDG09R
002325070420     C                   IF        oD34esito = *blanks
002326070420     C                   EXSR      EXEGRG
002327070420     C                   ENDIF
002328070420     C*
002329070420     C* Reperimento informazioni versioni giri consegna da FIGRV
002330070420     C                   IF        oD34esito = *blanks
002331070420     C                   EXSR      EXEGRV
002332070420     C                   ENDIF
002336070420     C                   ENDIF
002337070420     C*
002338070420     C* Valorizzazione buffer d output
002339070420     C                   IF        oD34esito = *blanks
002340070420     C                   EXSR      OUTINF1
002341070420     C                   ENDIF
002342070420     C*
002343070420     C                   WHEN      iD34tip = '2'
002344070416     C*
002345070416     C* Reperimento informazioni da FIARG
002346070416     C                   IF        oD34esito = *blanks
002347070416     C                   EXSR      EXEARG
002348070416     C                   ENDIF
002349070416     C*
002350070416     C* Reperimento informazioni anagrafico giri tramite driver FIDG09R
002351070416     C                   IF        oD34esito = *blanks
002352070416     C                   EXSR      EXEGRG
002353070416     C                   ENDIF
002354070416     C*
002355070416     C* Reperimento informazioni versioni giri consegna da FIGRV
002356070416     C                   IF        oD34esito = *blanks
002357070416     C                   EXSR      EXEGRV
002358070416     C                   ENDIF
002359070416     C*
002360070416     C* Valorizzazione buffer d output
002361070416     C                   IF        oD34esito = *blanks
002362070420     C                   EXSR      OUTINF2
002363070416     C                   ENDIF
002364070420     C                   ENDSL
002365070420     C                   ENDSL
002366070420     C*
002367070420     C* Verifico tipo elaborazione richiesta
002368070420     C                   SELECT
002369070420     C                   WHEN      iD34tla = *blanks
002370070420     C                   SETON                                        LR
002371070420     C*
002372070420     C                   WHEN      iD34tla = 'C'
002373070420     C                   SETON                                        LR
002374070420     C*
002375070420     C                   WHEN      iD34tla = 'E'
002376070420     C                   SETON                                        RT
002377070420     C                   ENDSL
014200951025     C*
014316070416     C*--------------------------------------------------------------------------------------------*
014317070416     C* INZOUT - Inizializzazione buffer d output
014318070416     C*--------------------------------------------------------------------------------------------*
014319070416     C     INZOUT        BEGSR
014320070416     C*
014321070420     C                   clear                   oD34ttl
014322070420     C                   clear                   oD34ttldes
014323070420     C                   clear                   oD34tgi
014324070420     C                   clear                   oD34tgides
014325070420     C                   clear                   oD34cgi
014326070420     C                   clear                   oD34cgides
014327070420     C                   clear                   oD34ver
014328070420     C                   clear                   oD34verdes
014329070420     C                   clear                   oD34idb
014330070420     C                   clear                   oD34sco
014331070420     C                   clear                   oD34datela
014332070420     C                   clear                   oD34oraela
014333070420     C                   clear                   oD34lno
014334070420     C                   clear                   oD34lnodes
014335070420     C                   clear                   oD34err
014336070420     C                   clear                   oD34errdes
014337070420     C                   clear                   oD34sts
014338070420     C                   clear                   oD34stsdes
014339070420     C                   clear                   oD34esito
014340070420     C*
014341070420     C                   clear                   fidg09ds
014342070420     C                   clear                   Out_KeyVer
014343070420     C                   clear                   Out_VerDes
014350070416     C*
014351070416     C                   ENDSR
014352070416     C*--------------------------------------------------------------------------------------------*
014353070416     C* EXEARG - Reperimento informazioni da FIARG
014354070416     C*--------------------------------------------------------------------------------------------*
014355070416     C     EXEARG        BEGSR
014356070416     C*
014357070416     C     KEYarg01_c    chain     fiarg01l
014358070416     C                   if        %found(fiarg01l)
014362070416     C                   else
014363070416     C                   eval      oD34esito = '1'
014364070416     C                   endif
014365070416     C*
014366070416     C                   ENDSR
014371070416     C*--------------------------------------------------------------------------------------------*
014372070416     C* EXEGRG - Reperimento informazioni anagrafico giri tramite driver FIDG09R
014373070416     C*--------------------------------------------------------------------------------------------*
014374070416     C     EXEGRG        BEGSR
014375070416     C*
014377070416     C                   clear                   fidg09ds
014378070416     C                   eval      d09iop0 = '001'
014379070416     C                   eval      d09ifgs = argPOC
014380070416     C                   eval      d09idat = argDTVTEL
014381070416     C                   eval      d09icgi = argCGI
014382070416     C                   eval      kpjbu = fidg09ds
014383070416     C                   call(e)   'FIDG09R'
014384070416     C                   parm                    kpjba
014385070416     C                   eval      fidg09ds = kpjbu
014393070416     C*
014394070416     C                   ENDSR
014395070416     C*--------------------------------------------------------------------------------------------*
014396070416     C* EXEGRV - Reperimento informazioni versioni giri consegna da FIGRV
014397070416     C*--------------------------------------------------------------------------------------------*
014398070416     C     EXEGRV        BEGSR
014399070416     C*
014410070416     C* Reperisco versioni giri x filiale/data richieste
014411070416     C                   call(e)   'FIDG11R1'
014412070416     C                   parm      argPOC        wPOC
014413070416     C                   parm      argDTVTEL     wDAT
014414070416     C                   parm      *blanks       Out_KeyVer        8
014415070416     C                   parm      *blanks       Out_VerDes       30
014416070416     C                   parm      *blanks       Out_Esito         1
014417070416     C*
014418070416     C                   ENDSR
014419070420     C*--------------------------------------------------------------------------------------------*
014420070420     C* OUTINF1 - Valorizzazione buffer d output x tipo lancio "1"
014421070420     C*--------------------------------------------------------------------------------------------*
014422070420     C     OUTINF1       BEGSR
014423070420     C*
014424070420     C                   eval      oD34cgi    = d09ocgi
014425070420     C                   eval      oD34cgides = d09odes
014426070420     C*
014427070420     C                   eval      oD34ver    = Out_KeyVer
014428070420     C                   eval      oD34verdes = Out_VerDes
014454070420     C*
014455070420     C                   eval      oD34lno    = iD34lno
014456070420     C                   select
014459070420     C                   when      oD34lno = 'V'
014460070420     C                   eval      oD34lnodes = 'LIVELLO VIA'
014461070420     C                   when      oD34lno = 'L'
014462070420     C                   eval      oD34lnodes = 'LIVELLO LOCALITA'
014463070420     C                   when      oD34lno = 'C'
014464070420     C                   eval      oD34lnodes = 'LIVELLO COMUNE'
014465070420     C                   when      oD34lno = 'P'
014466070420     C                   eval      oD34lnodes = 'LIVELLO PROVINCIA'
014467070420     C                   when      oD34lno = 'VA'
014468070420     C                   eval      oD34lnodes = 'VIA AMBIGUA'
014469070420     C                   when      oD34lno = 'LA'
014470070420     C                   eval      oD34lnodes = 'LOCALITA AMBIGUA'
014471070420     C                   when      oD34lno = 'CA'
014472070420     C                   eval      oD34lnodes = 'COMUNE AMBIGUO'
014473070420     C                   when      oD34lno = 'PA'
014474070420     C                   eval      oD34lnodes = 'PROVINCIA AMBIGUA'
014475070420     C                   when      oD34lno = 'N'
014476070420     C                   eval      oD34lnodes = 'MANCA CARTOGRAFIA'
014477070420     C                   other
014478070420     C                   eval      oD34lnodes = *all'*'
014480070420     C                   endsl
014481070420     C*
014482070420     C                   eval      oD34err    = iD34err
014483070420     C                   select
014486070420     C                   when      oD34err = '01'
014487070424     C                   eval      oD34errdes = 'ZONA GEO NON COPERTA DA GIRO'
014488070420     C                   when      oD34err = '02'
014489070420     C                   eval      oD34errdes = 'FILIALE GEO NON ATTIVA'
014490070420     C                   when      oD34err = '03'
014491070420     C                   eval      oD34errdes = 'SPEDIZIONE NON OTTIMIZZATA'
014492070420     C                   when      oD34err = '04'
014493070420     C                   eval      oD34errdes = 'PROFILO GEO NON ATTIVO'
014494070420     C                   when      oD34err = '05'
014495070420     C                   eval      oD34errdes = 'VERSIONE GEO GIRI NON TROVATA'
014496070420     C                   when      oD34err = '10'
014497070420     C                   eval      oD34errdes = 'GIRO REPERITO'
014498070420     C                   when      oD34err = '11'
014499070420     C                   eval      oD34errdes = 'GIRO REPERITO'
014500070420     C                   when      oD34err = '12'
014501070420     C                   eval      oD34errdes = 'GIRO REPERITO'
014502070420     C                   when      oD34err = '51'
014503070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN NORMALIZZAZ.'
014504070420     C                   when      oD34err = '52'
014505070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN OTTIMIZZAZ.'
014506070420     C                   when      oD34err = *blanks and iD34cgi = *blanks
014508070420     C                   eval      oD34errdes = 'GIRO NON REPERITO'
014509070420     C                   other
014510070420     C                   eval      oD34errdes = *blanks
014511070420     C                   endsl
014518070420     C*
014519070420     C                   eval      oD34sts    = *all'*'
014520070420     C                   eval      oD34stsdes = 'DATI INCONGRUENTI O MANUALI '
014522070420     C                   select
014524070420     C                   when      (iD34lno =  'V'   or
014525070420     C                              iD34lno =  'VA') and
014526070420     C                              iD34cgi <> *blanks
014527070420     C                   eval      oD34sts    = 'OK'
014528070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO VIA'
014529070420     C                   when      (iD34lno =  'L'   or
014530070420     C                              iD34lno =  'LA') and
014531070420     C                              iD34cgi <> *blanks
014532070420     C                   eval      oD34sts    = 'OK'
014533070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO LOCALITA'
014534070420     C                   when      (iD34lno =  'C'   or
014535070420     C                              iD34lno =  'CA') and
014536070420     C                              iD34cgi <> *blanks
014537070420     C                   eval      oD34sts    = 'OK'
014538070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO COMUNE'
014539070420     C                   when      (iD34lno =  'P'   or
014540070420     C                              iD34lno =  'PA') and
014541070420     C                              iD34cgi <> *blanks
014542070420     C                   eval      oD34sts    = 'OK'
014543070423     C                   eval      oD34stsdes = 'GIRO SI - LIVELLO PROVINCIA'
014544070420     C                   when      (iD34err = '02'  or
014545070420     C                              iD34err = '04') and
014546070420     C                              iD34cgi = *blanks
014547070420     C                   eval      oD34sts    = 'KO'
014548070420     C                   eval      oD34stsdes = 'ERRORE CONFIGURAZIONE GEO'
014549070420     C                   when      (iD34err = '51'  or
014550070420     C                              iD34err = '52') and
014551070420     C                              iD34cgi = *blanks
014552070420     C                   eval      oD34sts    = 'KO'
014553070420     C                   eval      oD34stsdes = 'ERRORE DI SISTEMA GEO'
014554070420     C                   when      iD34lno = 'N' and
014555070420     C                             iD34cgi = *blanks
014556070420     C                   eval      oD34sts    = 'KO'
014557070420     C                   eval      oD34stsdes = 'CARTOGRAFIA MANCANTE'
014558070420     C                   endsl
014566070420     C*
014567070420     C                   ENDSR
014568070416     C*--------------------------------------------------------------------------------------------*
014569070420     C* OUTINF2 - Valorizzazione buffer d output x tipo lancio "2"
014570070416     C*--------------------------------------------------------------------------------------------*
014571070420     C     OUTINF2       BEGSR
014572070416     C*
014573070416     C                   eval      oD34ttl    = argTTL
014574070416     C                   select
014575070416     C                   when      oD34ttl = '1'
014576070423     C                   eval      oD34ttldes = 'SI - CON GIRO'
014577070416     C                   when      oD34ttl = '2'
014578070423     C                   eval      oD34ttldes = 'SI - SENZA GIRO'
014579070416     C                   when      oD34ttl = *blanks
014580070423     C                   eval      oD34ttldes = 'NO ELAB. GEO'
014581070416     C                   endsl
014582070416     C*
014583070416     C                   eval      oD34tgi    = argTGI
014584070416     C                   select
014585070416     C                   when      oD34tgi = 'A'
014586070416     C                   eval      oD34tgides = 'AUTOMATICO'
014587070416     C                   when      oD34tgi = 'M'
014588070416     C                   eval      oD34tgides = 'MANUALE'
014589070416     C                   when      oD34tgi = *blanks
014590070416     C                   eval      oD34tgides = 'GESTIONE GIRO MANCANTE'
014591070416     C                   endsl
014592070416     C*
014593070416     C                   eval      oD34cgi    = argCGI
014594070416     C                   eval      oD34cgides = d09odes
014595070416     C*
014596070416     C                   eval      oD34ver    = Out_KeyVer
014597070416     C                   eval      oD34verdes = Out_VerDes
014598070416     C*
014599070416     C                   eval      oD34idb    = argIDB
014600070416     C                   eval      oD34sco    = argSCO
014601070416     C                   eval      oD34datela = argDTVTEL
014602070416     C                   eval      oD34oraela = argHVTEL
014603070416     C*
014604070416     C                   eval      oD34lno    = argLNO
014605070423     C***                select
014606070423     C***                when      oD34tgi = 'A'
014607070423     C                   select
014608070423     C                   when      oD34ttl <> *blanks
014609070416     C                   select
014610070416     C                   when      oD34lno = 'V'
014611070420     C                   eval      oD34lnodes = 'LIVELLO VIA'
014612070416     C                   when      oD34lno = 'L'
014613070420     C                   eval      oD34lnodes = 'LIVELLO LOCALITA'
014614070416     C                   when      oD34lno = 'C'
014615070420     C                   eval      oD34lnodes = 'LIVELLO COMUNE'
014616070416     C                   when      oD34lno = 'P'
014617070420     C                   eval      oD34lnodes = 'LIVELLO PROVINCIA'
014618070416     C                   when      oD34lno = 'VA'
014619070416     C                   eval      oD34lnodes = 'VIA AMBIGUA'
014620070416     C                   when      oD34lno = 'LA'
014621070416     C                   eval      oD34lnodes = 'LOCALITA AMBIGUA'
014622070416     C                   when      oD34lno = 'CA'
014623070416     C                   eval      oD34lnodes = 'COMUNE AMBIGUO'
014624070416     C                   when      oD34lno = 'PA'
014625070416     C                   eval      oD34lnodes = 'PROVINCIA AMBIGUA'
014626070416     C                   when      oD34lno = 'N'
014627070416     C                   eval      oD34lnodes = 'MANCA CARTOGRAFIA'
014628070416     C                   other
014629070423     C***                eval      oD34lnodes = *all'*'
014630070423     C                   eval      oD34lnodes = *blanks
014631070416     C                   endsl
014632070423     C                   endsl
014633070416     C*
014634070416     C                   eval      oD34err    = argERR
014635070423     C***                select
014636070423     C***                when      oD34tgi = 'A'
014637070423     C                   select
014638070423     C                   when      oD34ttl <> *blanks
014639070416     C                   select
014640070416     C                   when      oD34err = '01'
014641070424     C                   eval      oD34errdes = 'ZONA GEO NON COPERTA DA GIRO'
014642070416     C                   when      oD34err = '02'
014643070416     C                   eval      oD34errdes = 'FILIALE GEO NON ATTIVA'
014644070416     C                   when      oD34err = '03'
014645070416     C                   eval      oD34errdes = 'SPEDIZIONE NON OTTIMIZZATA'
014646070416     C                   when      oD34err = '04'
014647070416     C                   eval      oD34errdes = 'PROFILO GEO NON ATTIVO'
014648070420     C                   when      oD34err = '05'
014649070420     C                   eval      oD34errdes = 'VERSIONE GEO GIRI NON TROVATA'
014650070416     C                   when      oD34err = '10'
014651070416     C                   eval      oD34errdes = 'GIRO REPERITO'
014652070416     C                   when      oD34err = '11'
014653070416     C                   eval      oD34errdes = 'GIRO REPERITO'
014654070416     C                   when      oD34err = '12'
014655070416     C                   eval      oD34errdes = 'GIRO REPERITO'
014656070416     C                   when      oD34err = '51'
014657070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN NORMALIZZAZ.'
014658070416     C                   when      oD34err = '52'
014659070420     C                   eval      oD34errdes = 'ERRORE SISTEMA IN OTTIMIZZAZ.'
014660070424     C                   when      oD34err = *blanks and
014661070424     C                             (argCGI = *blanks or argTTL='2')
014662070416     C                   eval      oD34errdes = 'GIRO NON REPERITO'
014663070416     C                   other
014664070416     C                   eval      oD34errdes = *blanks
014665070423     C                   endsl
014666070510     C                   when      oD34tgi = 'M'     and oD34cgi <> *blanks
014667070510     C                   eval      oD34err    = *blanks
014668070510     C                   eval      oD34errdes = 'GIRO ATTRIBUITO MANUALMENTE'
014671070510     C                   when      oD34tgi = *blanks
014672070416     C                   eval      oD34err    = *blanks
014673070416     C                   eval      oD34errdes = 'GESTIONE GIRO MANCANTE'
014674070416     C                   endsl
014675070416     C*
014676070420     C                   eval      oD34sts    = *all'*'
014677070420     C                   eval      oD34stsdes = 'DATI INCONGRUENTI O MANUALI '
014678070416     C                   select
014679070416     C                   when      oD34tgi = 'A'
014680070416     C                   select
014681070416     C                   when      (argLNO =  'V'   or
014682070416     C                              argLNO =  'VA') and
014683070416     C                              argCGI <> *blanks
014684070416     C                   eval      oD34sts    = 'OK'
014685070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO VIA'
014686070416     C                   when      (argLNO =  'L'   or
014687070416     C                              argLNO =  'LA') and
014688070416     C                              argCGI <> *blanks
014689070416     C                   eval      oD34sts    = 'OK'
014690070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO LOCALITA'
014691070416     C                   when      (argLNO =  'C'   or
014692070416     C                              argLNO =  'CA') and
014693070416     C                              argCGI <> *blanks
014694070416     C                   eval      oD34sts    = 'OK'
014695070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO COMUNE'
014696070416     C                   when      (argLNO =  'P'   or
014697070416     C                              argLNO =  'PA') and
014698070416     C                              argCGI <> *blanks
014699070416     C                   eval      oD34sts    = 'OK'
014700070423     C                   eval      oD34stsdes = 'GIRO AUT. - LIVELLO PROVINCIA'
014701070416     C                   when      (argERR = '02'  or
014702070416     C                              argERR = '04') and
014703070416     C                              argCGI = *blanks
014704070416     C                   eval      oD34sts    = 'KO'
014705070416     C                   eval      oD34stsdes = 'ERRORE CONFIGURAZIONE GEO'
014706070416     C                   when      (argERR = '51'  or
014707070416     C                              argERR = '52') and
014708070416     C                              argCGI = *blanks
014709070416     C                   eval      oD34sts    = 'KO'
014710070416     C                   eval      oD34stsdes = 'ERRORE DI SISTEMA GEO'
014711070416     C                   when      argLNO = 'N' and
014712070416     C                             argCGI = *blanks
014713070416     C                   eval      oD34sts    = 'KO'
014714070416     C                   eval      oD34stsdes = 'CARTOGRAFIA MANCANTE'
014715070416     C                   endsl
014716070416     C                   when      oD34tgi = 'M'
014717070416     C                   eval      oD34sts    = 'OK'
014718070424     C                   eval      oD34stsdes = 'GIRO ATTRIBUITO MANUALMENTE'
014719070416     C                   when      oD34tgi = *blanks
014720070416     C                   eval      oD34sts    = 'KO'
014721070416     C                   eval      oD34stsdes = 'GESTIONE GIRO MANCANTE'
014722070416     C                   endsl
014723070416     C*
014724070416     C                   ENDSR
014725070416     C*--------------------------------------------------------------------------------------------*
014726070416     C* *PSSR  - Gestione errori imprevisti
014727070416     C*--------------------------------------------------------------------------------------------*
014728070416     C     *PSSR         BEGSR
014729070416     C*
014730070416     C                   eval      oD34esito = '2'
014731070416     C*
014732070416     C                   seton                                        LR
014733070416     C*
014734070416     C                   ENDSR     '*CANCL'
014735070330     C*--------------------------------------------------------------------------------------------*
014736070330     C* *inzsr - operazioni iniziali
014737070330     C*--------------------------------------------------------------------------------------------*
014738070330     C     *inzsr        BEGSR
014739070330     C*
014740070416     C* Definizione chiavi
014741070416     C*
014742070416     C* Chiave su FIARG01L - Completa
014743070416     C     KEYarg01_c    KLIST
014744070416     C                   KFLD                    iD34aas
014745070416     C                   KFLD                    iD34lnp
014746070416     C                   KFLD                    iD34nrs
014747070416     C                   KFLD                    iD34nsp
014748070330     C*
014749070330     C                   ENDSR
