000100051229      *PARMS CLOSQLCSR(*ENDMOD)
000200051229      *PARMS DYNUSRPRF(*OWNER)
000300051227      *-----------------------------------------------------------------*
000400091029     H* STATISTICA DI CONTROLLO NUMEROSITÀ LANCIO AZIONI
000500051227      *-----------------------------------------------------------------*
000600091029     fkplog00f  if a e             disk
000700091030     Fkfazn11l  IF   E           K DISK
000800091102     Fkpprf01l  IF   E           K DISK
000900091102     Fazorg01l  IF   E           K DISK
001000091102     fTISEN1P   o    E             printer oflind(*in80)
001100051227
001200091029     d datdti7         s              7  0
001300091029     d datdtf7         s              7  0
001400091030     d Wtipo           s              1
001500091030     d DECTIPO1        s             12
001600091030     d DECTIPO2        s             13
001700091030     d wfil            s              3
001800091030     d wute            s             10
001900091030     d savute          s             10
002000091102     d savFIL          s              3
002100091030     d wazn            s              4
002200091030     d savazn          s              4
002300091102     d WOK             s              1
002400091103     d Wtpaz           s              1
002500091030     d contaG          s              5  0
002600091030     d contaP          s                   like(ContaG)
002700091030     d Pconta          s                   like(ContaG)
002800091029     d dataiso         s               d   datfmt(*iso)
002900091030     d dataeur         s               d   datfmt(*eur)
003000091029
003100051229     d wrkgetlista     s           4096    varying
003200051227
003300051227     d kpjba         e ds
003400091029     D*---------------------
003500091029     D* DS PARAMETRI DI LANCIO
003600091029     D*------------------
003700091029     D TISEN0DS        DS                  INZ
003800091029     D  PARDTI                 1      8  0
003900091029     D  PARDTF                 9     16  0
004000091029     D  PARtipo               17     17
004100091030     D  Pazn                  18     97    dim(20)
004200091030     D  PARfrg                98    157    dim(20)
004300091030     D  PARfrp               158    217    dim(20)
004400091029     d
004500091029     D TIBS34DS      E DS                                                       *Profili utente
004600091029     D DDATIUTE      E DS                                                       *Dati utente
004700091029     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
004800051227
004900091102     D TRMK13DS      E DS
005000091102     d
005100091102     d
005200091102     D PARta08         DS
005300091102     D* CODICE FILIALE
005400091102     D  t08FIL                 1      3  0
005500091102     D* SCELTA ORDINAMENTO STAMPA: 'C'=CODICE  'A'=RAGIONE SOCIALE
005600091102     D  T08SC1                 4      4
005700091102     D* RICHIESTA CLIENTI NON ABILITATI ALLA SPEDIZIONE
005800091102     D  T08SC2                 5      5
005900091102     D* RICHIESTA VOLUME AUTOMATICO
006000091102     D  T08SC3                 6      6
006100091102     D* RICHIESTA CODICE IMPORTANZA CLIENTE
006200091102     D  T08CIC                 7      7
006300091102      * tipo utilizzo peso VDL
006400091102     d  T08tap                 8      8
006500091102     D* NETWORK TARIFFE
006600091102     D  T08NTR                 9      9
006700091102     D* RICHIESTA TARIFFE CON ASSICURAZIONE PER CONTO
006800091102     D  T08SAC                10     10
006900091102     D* RICHIESTA ISTAT
007000091102     D  T08IST                11     11
007100091102      * flag rinuncia ac base
007200091102     d  T08racb               12     12
007300091102      * tariffa part. AC BASE
007400091102     d  T08base               13     13
007500091102     D* RICHIESTA TARIFFE bloccate
007600091102     D  T08blk                14     14
007700110923      * tariffa particolare
007800110923     d  T08ctp                15     15
007900091102     D* PARZIALIZZAZIONE PER CATEGORIA SEAT
008000091102     D  T08SNC                22     22
008100091102     D* CODICE CATEGORIA SEAT
008200091102     D  T08SCT                23     27  0
008300091103     D* non più usate!!
008400091103     D**T08SNM                28     28
008500091103     D**T08SCM                29     32
008600091102     D* RICHIESTA TIPO SERVIZIO
008700091102     D  T08TSP                33     33
008800091102     D* RICHIESTA TIPO TARIFFA
008900091102     D  T08TTR                94     94
009000091102     D* RICHIESTA CODICE PARTICOLARITA'
009100091102     D  T08CPA                95     96
009200091102     D* RICHIESTA TARIFFE CON CONSEGNA DDT
009300091102     D  T08BAM                97     97
009400091102     D* RICHIESTA TIPO STAMPA
009500091102     D  T08TPS                98     98
009600091102     D* Stampa Note Clienti
009700091102     D  t08TPN                99    100
009800091102     D* richiesta  Note Clienti
009900091102     D  t08nOTE              101    101
010000091102     D  t08TNOT              102    103
010100091102     d* Network
010200091102     D  T08NTW               104    106
010300091102     D* Codice agente
010400091102     D  T08AGE               107    113
010500091102     D  T08CNO               114    114
010600091102     D* Intervallo mese/anno per richiesta clienti non operativi
010700091102     D  T08DMM               115    116  0
010800091102     D  T08DAA               117    118  0
010900091102     D  T08AMM               119    120  0
011000091102     D  T08AAA               121    122  0
011100091102     d  T08bva               123    123
011200091102      * dati necessari x il caricamento della sk fig
011300091102     d  t08abi               124    125
011400091102     d  t08cdi               126    126
011500091102     d  t08car               127    129  0
011600091102     d  t08cpo               130    132  0
011700091102     D* parzializza tipo pagamento C/assegni
011800091102     D  T08tpc               133    133
011900091102     d* Luogo
012000091102     D  t08luog              134    134
012100091102     D  t08Tluo              135    137
012200091102     D*
012300051227      *-----------------------------------------------------------------*
012400091029     c                   eval      dataiso=%date(pardti:*iso)
012500091029     c                   eval      datdti7=%dec(dataiso:*cymd)
012600091029     c
012700091029     c                   eval      dataiso=%date(pardtf:*iso)
012800091029     c                   eval      datdtf7=%dec(dataiso:*cymd)
012900051229
013000060104     c                   exsr      PREPASQL
013100060104     c
013200091030     c* se non ha stampato nulla scrivo: nessuna anomalia riscontrata
013300091030     c                   if        dectipo1=*blanks
013400091102     c                   write     Sen1pnulla
013500091030     c                   else
013600091102     c                   write     Sen1pfine
013700091030     c                   endif
013800091030     c
013900051229     c                   Eval      *InLr = *On
014000051229
014100051229      *-----------------------------------------------------------------*
014200060104      * Lettura file fasi
014300051229      *-----------------------------------------------------------------*
014400060104     c     prepasql      begsr
014500051229
014600051229      *?Imposto la parte fissa dell'istruzione sql
014700051229     c                   eval      wrkgetlista =
014800091030     c                             'select  substr(logkpj, 4, 3),  -
014900091030     c                                      substr(logkpj, 1, 10),  -
015000091030     c                              substr(logkpj, 60, 4), -
015100091102     c                              logdai, logori, logdaf, logorf, -
015200091102     c                              logkpj  from kplog00f where -
015300091030     c                               logdai between '+
015400091030     c                               %editc(datdti7:'X') + ' and ' +
015500091030     c                               %editc(datdtf7:'X') + ' and   +
015600091030     c                               substr(logkpj, 60, 4) in('
015700091030      * ciclo sui codici azioni
015800091030     c                   Do        20            XX                4 0
015900091030     c                   If        pazn(XX) = *blanks
016000091030     c                   Leave
016100091030     c                   EndIf
016200091030     c
016300091030     c                   If        XX >1
016400091030     c                   eval      wrkgetlista =
016500091030     c                             %trim(wrkgetlista) + ','
016600091030     c                   EndIf
016700091030     c
016800091030     c                   eval      wrkgetlista =
016900091030     c                             %trim(wrkgetlista) + ' ''' +
017000091030     c                             pazn(xx) + ''''
017100091030     c                   EndDo
017200091030     c
017300091030     c                   Eval      wrkgetlista =
017400091030     c                             %trim(wrkgetlista) + ')'
017500051229      *?Ordinamento
017600051229     c                   eval      wrkgetlista = wrkgetlista +
017700091030     c                             ' order by substr(logkpj, 4, 3), +
017800091030     c                             substr(logkpj, 1, 10), +
017900091030     c                             substr(logkpj, 60, 4)'
018000051229      *?Solo lettura
018100051229     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
018200051229
018300051229     C/EXEC SQL
018400051229     C+ PREPARE s1 FROM :wrkgetlista
018500051229     C/END-EXEC
018600051229     C
018700051229     C/EXEC SQL
018800051229     C+ DECLARE A1 CURSOR FOR S1
018900051229     C/END-EXEC
019000051229     C
019100051229     C/EXEC SQL
019200051229     C+ OPEN a1
019300091030     c/END-EXEC
019400051229
019500051229      *?Leggo il file
019600051229     c                   do        *hival
019700051229     C/EXEC SQL
019800091030     C+ FETCH NEXT FROM a1 INTO :wfil, :wute, :wazn, :logdai, :logori, :logdaf,
019900091102     C+                         :logorf, :logkpj
020000051229     C/END-EXEC
020100051229
020200051229     c                   select
020300051229
020400051229     c                   when      sqlcod = 100
020500051229     c                   leave
020600051229     c                   when      sqlcod < 0
020700051229     c                   seton                                        H1
020800051229     c                   exsr      srfine
020900051229     c                   other
021000060104     c
021100091030     c                   exsr      Elabora
021200060104     c
021300051229     c                   endsl
021400051229
021500051229     c                   enddo
021600091030     c
021700091030     c* Eseguo ultimo controllo
021800091030     c                   clear                   wute
021900091030     c                   clear                   wazn
022000091030     c                   exsr      Verifica
022100091030     c
022200051229
022300051229     C/EXEC SQL
022400051229     C+ CLOSE a1
022500051229     C/END-EXEC
022600051229
022700051229     c                   endsr
022800051229
022900051229      *-----------------------------------------------------------------*
023000051229      * Fine programmma con codice errore
023100051229      *-----------------------------------------------------------------*
023200051229     c     srfine        begsr
023300051229
023400051229     c                   eval      *InLr = *On
023500051229     c                   return
023600051229
023700051229     c                   endsr
023800091030      *-----------------------------------------------------------------*
023900091030      * Elabora azione per azione per ogni utente
024000091030      *-----------------------------------------------------------------*
024100091030     c     Elabora       begsr
024200091102     c                   clear                   Wok
024300091102     C                   SELECT
024400091102     c                   when      wazn='MK13'
024500091102     c                   exsr      CtrMK13
024600091102     c
024700091102     c                   when      wazn='TA08'
024800091102     c                   exsr      CtrTA08
024900091102     c                   other
025000091102     c                   eval      wok='S'
025100091102     c                   ENDSL
025200091030     c
025300091102     c                   if        wok='S'
025400091102     c
025500091030     c* A  cambio utente, azione --> verifico la frequenza
025600091030     c                   if        savute=*blanks or savute<>wute
025700091030     c                   if        savute<>*blanks
025800091030     c                   exsr      verifica
025900091030     c                   endif
026000091030     c                   eval      savute=wute
026100091030     c                   eval      savazn=wazn
026200091102     c                   eval      savFIL=wFIL
026300091030     c                   clear                   contaG
026400091030     c                   clear                   contaP
026500091030     c                   endif
026600091030     c
026700091030     c                   if        savazn=*blanks or savazn<>wazn
026800091030     c                   if        savazn<>*blanks
026900091030     c                   exsr      verifica
027000091030     c                   endif
027100091030     c                   eval      savute=wute
027200091030     c                   eval      savazn=wazn
027300091102     c                   eval      savFIL=wFIL
027400091030     c                   clear                   contaG
027500091030     c                   clear                   contaP
027600091030     c                   endif
027700091030     c
027800091030     c* coteggio nel periodo
027900091030     c                   eval      contaP=contaP+1
028000091030     c* conteggio giornaliero
028100091030     c                   if        logdai=datdtf7
028200091030     c                   eval      contaG=contaG+1
028300091030     c                   endif
028400091102     c
028500091102     c                   endif
028600091030     c
028700091030     c                   endsr
028800091102      *-----------------------------------------------------------------*
028900091102      * controlla kpjbu azione MK13
029000091102      *-----------------------------------------------------------------*
029100091102     c     CtrMK13       begsr
029200091102     c                   eval      trmk13ds=%subst(logkpj:247:256)
029300091103     c                   eval      wtpaz   =%subst(logkpj:64:1)
029400091103     c
029500091103     c*     Considero solo se lancio Batch (perescludere i tentativi interattivi
029600091103     c*      di Nicola!!)
029700091103     c                   if        wtpaz='B'
029800091102     c
029900091103     c* non considero se parzializzato troppo
030000091102     c                   if        d13cap=*blanks  and
030100091102     c                             d13loc=*blanks  and
030200110923     c                             d13cmmu=0       and
030300091102     c                             d13sct=0        and
030400110923     c                             d13cic=*blanks  and
030500091102     c                             d13cmm=0        and
030600110923     c                             d13cco=*blanks  and
030700110923     c                             d13dadda=0      and
030800110923     c                             d13dada=0       and
030900091102     c                             d13ifoal=0      and
031000091102     c                             d13nifo=' '     and
031100091102     c                             d13rip=*blanks  and
031200091102     c                             d13ripa=0       and
031300091102     c                             d13pri=' '      and
031400091102     c                             d13gdo=' '      and
031500091102     c                             d13ecm=' '      and
031600100608     c                             d13hpr=' '      and
031700110923     c                             d13sbx=' '      and
031800091102     c                             d13pesa=0       and
031900091102     c                             d13pspa=0       and
032000091102     c                             d13conc=*blanks and
032100110923     c                             d13moi =*blanks and
032200091102     c                             d13logi=*blanks and
032300091102     c                             d13logo=*blanks and
032400110923     c                             d13logp=*blanks
032500091102     c                   eval      wok='S'
032600091102     c                   endif
032700091103     c                   endif
032800091102     c
032900091102     c                   endsr
033000091102      *-----------------------------------------------------------------*
033100091102      * controlla kpjbu azione TA08
033200091102      *-----------------------------------------------------------------*
033300091102     c     CtrTA08       begsr
033400091102     c                   eval      parta08 =%subst(logkpj:247:256)
033500091102     c                   if        T08sc3='N'      and
033600091102     c                             t08cic=' '      and
033700091102     c                             t08tap=' '      and
033800091102     c                             t08tap=' '      and
033900091102     c                             t08ntr='N'      and
034000091102     c                             t08sac='N'      and
034100091102     c                             t08ist='N'      and
034200091102     c                             t08racb='N'      and
034300091102     c                             t08base='N'      and
034400091102     c                             t08blk ='N'      and
034500110923     c                             t08ctp =' '      and
034600091102     c                             t08tsp =' '      and
034700091102     c                             t08ttr =' '      and
034800091102     c                             t08cpa ='  '      and
034900091102     c                             t08bam =' '      and
035000091102     c                             t08note='N'      and
035100091102     c                             t08age<= *zeros  and
035200091102     c                             t08sc2 ='N'      and
035300091102     c                             t08cno ='S'      and
035400091102     c                             t08tpc ='N'      and
035500091102     c                             t08luog='N'
035600091102     c                   eval      wok='S'
035700091102     c                   endif
035800091102     c
035900091102     c                   endsr
036000091030      *-----------------------------------------------------------------*
036100091030      * Verifico se utente/azione da stampare in base alla frequenza
036200091030      *-----------------------------------------------------------------*
036300091030     c     Verifica      begsr
036400091030     c                   z-add     1             xx
036500091030     c     savazn        lookup    pazn(xx)                               30
036600091030     c
036700091030     c                   if        *in30
036800091030     c                   movel     parfrg(xx)    wfreqg            3 0
036900091030     c                   movel     parfrp(xx)    wfreqp            3 0
037000091030     c
037100091030     c                   if        contag>wfreqg  or
037200091030     c                             (contap>wfreqp  and partipo='R')
037300091030     c                   exsr      Stampa
037400091030     c                   endif
037500091030     c
037600091030     c                   endif
037700091030     c                   endsr
037800091030      *-----------------------------------------------------------------*
037900091030      * Stampo l'utente che ha utilizzato più del dovuto l'azione
038000091030      *-----------------------------------------------------------------*
038100091030     c     Stampa        begsr
038200091030     c* Stampa testata
038300091102     c                   if        *in80
038400091102     c                   write     sen1pt1
038500091102     c                   write     sen1pt3
038600091102     c                   setoff                                       80
038700091030     c
038800091030     C                   EVAL      DECTIPO1='NEL GIORNO :'
038900091030     C                   EVAL      DECTIPO2='NEL PERIODO :'
039000091030     c                   endif
039100091030     c
039200091102     C* SE il campo filiale è numerico decodifico, altrimenti
039300091102     c*  impsoto 046
039400091102     c                   clear                   p1cfil
039500091102     c* se profilo utente lungo  6 utente di sede
039600091102     c                   select
039700091102     c                   when      %subst(savute:7:1)=' '
039800091102     c                   eval      p1cfil=046
039900091102     c
040000091102     c                   when      savfil>*zeros
040100091102     c                   movel     savfil        p1cfil
040200091102     c                   other
040300091102     c                   eval      p1cfil=046
040400091102     c                   endsl
040500091102     c
040600091102     c     p1cfil        chain     azorg01l
040700091102     c                   if        %found(azorg01l)
040800091102     c                   movel     orgdes        p1dfil
040900091102     c                   else
041000091102     c                   eval      p1dfil=*all'?'
041100091102     c                   endif
041200091102     c
041300091102     c* Decodifico utente
041400091102     c     savute        chain     kpprf01l
041500091102     c                   if        %found(kpprf01l)
041600091102     c                   movel     prftxt        p1dute
041700091102     c                   else
041800091102     c                   eval      p1dute=*all'?'
041900091102     c                   endif
042000091102     c
042100091102     c                   write     Sen1pd1
042200091030     c
042300091030     c                   endsr
042400051227
042500051227      *-----------------------------------------------------------------*
042600051227      * Routine iniziale
042700051227      *-----------------------------------------------------------------*
042800051227     c     *inzsr        begsr
042900051227
043000091029     C*--------------------
043100091029     C* RICEVIMENTO PARAMETRI
043200091029     C*--------------------
043300091029     C     *ENTRY        PLIST
043400091029     C                   PARM                    KPJBA
043500091029     c                   movel     kpjbu         TISEN0DS
043600091029
043700091029     C                   CLEAR                   TIBS34DS
043800091029     C                   CLEAR                   AZUTEDS
043900091029     C                   CLEAR                   DDATIUTE
044000091029     C*
044100091029     C     *DTAARA       DEFINE    §azute        azuteds
044200091029     C     *DTAARA       DEFINE    §datiute      ddatiute
044300091029     C                   IN(E)     *DTAARA
044400091029if  1C                   IF        %Error
044500091029     c                   EVAL      I34Tla = 'L'
044600091029     C                   CALL      'TIBS34R'
044700091029     C                   PARM                    Tibs34Ds
044800091029     C                   IN        *DTAARA
044900091029e   1C                   ENDIF
045000091029
045100091029     C*
045200091030     c* Dati richiesti
045300091030     c                   clear                   dectipo1
045400091030     c                   clear                   dectipo2
045500091030     c
045600091030     c     *iso          movel     pardti        dataeur
045700091030     c                   movel     dataeur       gmadal            8 0
045800091030     c
045900091030     c     *iso          movel     pardtf        dataeur
046000091030     c                   movel     dataeur       gmaal             8 0
046100091030     c
046200091030     c* Stampa pagina selezioni
046300091102     c                   write     sen1pt1
046400091030     c
046500091030     c* Decodifico azioni scelte
046600091030     C                   SETON                                        02
046700091030     c                   z-add     1             xx
046800091030     c                   dow       xx<=20 and pazn(xx)<>*blanks
046900091030     c     pazn(xx)      chain     kfazn11l
047000091030     c                   if        %found(kfazn11l)
047100091030     c                   movel     kdsaz         p1dazn
047200091030     c                   else
047300091030     c                   eval      p1dazn='?'
047400091030     c                   endif
047500091030     c                   eval      p1cazn=pazn(xx)
047600091030     c
047700091030     c* frequenza giornaliera
047800091030     c                   movel     parfrg(xx)    p1cfrg            3
047900091030     c
048000091030     c                   if        partipo='R'
048100091030     c                   seton                                        01
048200091030     c* frequenza del periodo
048300091030     c                   movel     parfrp(xx)    p1cfrp            3
048400091030     c                   endif
048500091030     c
048600091102     c                   write     Sen1pt2
048700091030     C                   SETOFF                                       02
048800091030     c
048900091030     C                   ADD       1             XX
049000091030     C                   enddo
049100091030     c
049200091102     c                   seton                                        80
049300091029     C                   ENDSR
