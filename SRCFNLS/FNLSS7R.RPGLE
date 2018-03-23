000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200130701     H* FNLSS7R *----------------------------------------------------*
000300130701     H*         - Spedizioni spareggiate - Batch                     *
000500000000     H*--------------------------------------------------------------*
000600900509     FAZORG01L  IF   E           K DISK
001000130701     FFNBLP79L  IF   E           K DISK
001300130703     FFNBLT01L  iF   E           K DISK
001400070111     FFNBRV01L  IF   E           K DISK
001500940920     FFNFGV01L  IF   E           K DISK
001600000427     FFNFGW01L  IF   E           K DISK
001700900601     FTABEL00F  IF   E           K DISK
001800120203     FFNBRV07L  IF   E           K DISK    rename(fnbrv000:fnbrv07)
002000130701     Ffiapd01l  IF   E           K DISK
002100130701     Ffiars01l  IF   E           K DISK
002400130703     Ffnlss7p   O    e             PRINTER
002500940920     D*
003400020807     D KPJBA         E DS
003601130703     d Tibs34ds      e ds                  inz
003602130703     d §azute        e ds                  Extname(Azute00f)
003603130703     d                                     dtaara
003604130703     d §Datiute      e ds                  EXTname(ddatiute)
003605130703     d                                     dtaara
003800020807     D DS1B          E DS
004800120203     D FNLv53DS      E DS
005300940920     D*
005600900511     D PARAM           DS
005700900615     D  VIDFFV                 1      3  0
006400130701     D  VIDNFV                 4      8  0
007200020807      *
008600960319     D                 DS
008700960319     D  FGVFFV                 1    240
008800960319     D  FGVFF2               241    447
008900000427     D  FGWFF3               448    687
009000000427     D  FGWFF4               688    897
009100000427     D  VUOTO                898    900
009200000427     D  FFV                    1    900
009300000427     D                                     DIM(300)
010400020807      *
010500020807      * Variabili
010600020807      *
010700020807     D KNPG            S                   LIKE(BRVNPG)
010800020807     D KFGS            S                   LIKE(BRVFGS)
010900130703     D wnfv60          S                   LIKE(BRVNFV)
011000020807     D COD             S                   LIKE(TBLCOD)
011100020807     D KEY             S                   LIKE(TBLKEY)
011300020807     D KNFV            S                   LIKE(FGVNFV) INZ
011600120202     D kpdpdr          S                   LIKE(APDpdr)
011700120202     D kpdtip          S                   LIKE(APDtip)
011800120203     d WTROV           s              1
011900120203     d prtdfv          s                   like(d53dfv)
012000120203     d prtnfv          s                   like(d53nfv)
012100120203     d prtnpg          s                   like(d53npg)
012200120203     d prtspg          s                   like(d53spg)
012300120203     d*prtznc          s                   like(brvznc)
012400120203     d prtfgs          s                   like(d53fgs)
012500120203     d wdfs            s                   like(brvdfs)
012600120203     d whms            s                   like(brvhms)
012700120203     d ktrcars         s                   like(arstrc) inz('C')
012900120202     D DESCPR          S             15    inz
013000120202     d dataiso         s               d   datfmt(*iso)
013100120202     d $righe          s              2  0
013200120202     d uncollo         s              1
013300020807     D WLNABO          S              1    INZ
013900020807     D W003A           S              3    INZ
014100020807     D WTIME           S             14  0
014200020807     D WDATE           S              8  0
014500020807     D XX              S              3  0 INZ
014501130808     d codut           s              1  0 inz(1)
014502130703      // ? PROTOTIPI ?
014503130703      /copy gaitrasrc/srcprotopr,tibs34r
015100900521     C****************************************************************
015200900521     C*  RIEPILOGO INDICATORI
015300900521     C***************************************************************
016100900521     C* 30    - LETTURA FILE BOLLE
016200900521     C* 31/32 - DI COMODO RIUTILIZZABILI
016300900521     C*****************************************************************
016400000000     C     *ENTRY        PLIST
016500000000     C                   PARM                    KPJBA
016600910827     C                   MOVEL     KPJBU         PARAM
016700000114     C                   Z-ADD     VIDNFV        WNFV60
017400941007     C*---------------------------------------------------------------*
017500001130     C* RESET FLAG ESISTENZA LNA DA BORDERIZZARE
017600001130     C*
017700020807     C                   MOVE      ' '           WLNABO
017800001130     C*
017900900615     C* IMPOSTAZIONI INIZIALI
018000900615     C                   EXSR      IMPO
018100001130     C*
018601130703
019300120203     c                   eval      utime= %dec(%time())
019400120202     c                   eval      ptitolo2='Linea'
019500120202     c                   eval      ptitolo2=%trim(ptitolo2)+' '+
019600120202     c                             %editc(simfel:'X')+'/'+
019700120202     c                             %editc(vidffv:'X')+' '+ deslnp +
019800120202     c                             ' ' + deslna + ' Foglio Viaggio n.' +
019900120202     c                             %editc(vidnfv:'X')
020000120202     c                   write     ls37t00
020100120206     c                   eval      $righe=7
020101130808
020102130808     C* SE LNA DA elaborare  non E' PRESENTE IN FV ESCO DAL PROGRAMMA
020103130808     c* esco anche se foglio viaggio già chiuso
020104130808     C*
020105130808     C                   IF        wlnabo<>' '
020106130808     c                   write     ls37nolna
020108130808     C                   GOTO      FINE
020109130808     C                   ENDIF
020110130808     C                   IF        fgvfcf<>*blanks
020112130808     c                   write     ls37chiuso
020113130808     C                   GOTO      FINE
020114130808     C                   ENDIF
020300001130     C*
020400941007     C     KBLP          SETLL     FNBLP000
020700130703     C     KBLP          READE     FNBLP000
020701130703     c                   dow       not %eof(fnblp79l)
020800940928     C*
020900130703     C* elaboro solo le spedizioni non borderizzate
020901130703     c                   if        blpdbr=0
022200130703     C* Controllo se eiste la spunta del segnacollo
022300900618     C                   EXSR      CTRNSC
022301130703     c* Se esiste almeno una spunta verifico e stampo se spedizione spareggiata
022303130703     c                   if        uncollo='S'
022304130703     c                   exsr      stampa
022305130703     c                   endif
022306130703     c                   endif
022307130703     C     KBLP          READE     FNBLP000
022308130703     c                   enddo
028900940920     C*
029100950712     C*
033000120203     C                   CLEAR                   FNLV53DS
033100120203     C                   MOVEL     'C'           D53TLA
033200120203     C                   CALL      'FNLV53R'
033300120203     C                   PARM                    FNLV53DS
033301130808
033302130808     c     fine          tag
033400120202     c
033600120202     c                   write     fines
034600120213     C*
034700000000     C                   SETON                                        LR
034800941007     C**************************************************************************
034900941007     C*    *INZSR
035000941007     C**************************************************************************
035100941007     C     *INZSR        BEGSR
035200941007     C     KBLT1         KLIST
035300941007     C                   KFLD                    BLPAAS
035400941007     C                   KFLD                    BLPLNP
035500941007     C                   KFLD                    BLPNRS
035600941007     C                   KFLD                    BLPNSP
035700000114     C     KBRV1         KLIST
035800941007     C                   KFLD                    KNPG
035900000114     C                   KFLD                    WNFV60
036000941007     C                   KFLD                    KFGS
036100941007     C                   KFLD                    BLPFLS
036200941007     C                   KFLD                    VIDFFV
036300941007     C                   KFLD                    BLPNRS
036400941007     C                   KFLD                    BLTNSC
036500120203     C     KBRV7         KLIST
036600120203     C                   KFLD                    bltfls
036700120203     C                   KFLD                    bltlna
036800120203     C                   KFLD                    bltnrs
036900120203     C                   KFLD                    bltnsc
037000120203     C     Kars          KLIST
037100120203     C                   KFLD                    bltfls
037200120203     C                   KFLD                    bltlna
037300120203     C                   KFLD                    bltnrs
037400120203     C                   KFLD                    bltnsc
037500120203     c                   kfld                    ktrcars
037600941007     C* ACCESSO   FILE TABEL00F
037700941007     C     KTAB1         KLIST
037800941007     C                   KFLD                    CODUT
037900941007     C                   KFLD                    COD
038000941007     C                   KFLD                    KEY
039100941007     C* PER BOLLE
039200941007     C     KBLP          KLIST
039300020807     C                   kfld                    SIMfel
039400941007     C                   KFLD                    KNFV
039500941007     C                   KFLD                    VIDFFV
039600020807     C     K02FGV01      klist
039700020807     C                   kfld                    VIDnfv
039800020807     C                   kfld                    SIMfel
039900120202     C     Kfapd         klist
040000120202     C                   kfld                    kpdtip
040100120202     C                   kfld                    kpdpdr
040200941007     C*
040300941007     C                   ENDSR
040400940920     C**************************************************************************
040500940920     C*    IMPOSTAZIONI INIZIALI
040600940920     C**************************************************************************
040700900615     C     IMPO          BEGSR
043100940920     C*
043101130703     c                   exsr      DatiJob
043102130703
043200900615     C* DECODIFICA LINEA PARTENZA
043300940920     C     SIMFEL        CHAIN     AZORG01L                           31
043400900615     C  N31              MOVEL     ORGDES        DESLNP
043500900615     C   31              MOVEL     *BLANKS       DESLNP           20
043600940920     C*
043700900615     C* DECODIFICA LINEA ARRIVO
043800940920     C     VIDFFV        CHAIN     AZORG01L                           31
043900900615     C  N31              MOVEL     ORGDES        DESLNA
044000900615     C   31              MOVEL     *BLANKS       DESLNA           20
045000940920     C*
045001130703     C                   TIME                    WTIME
045002130703     C                   MOVE      WTIME         WDATE
047800940920     C*
047900950828     C* CHAIN SU F.VIAGG. PER CONTROLLARE CHE NON SIA CHIUSO O ANNULLATO
048000000427     C*
048200020807     C     K02FGV01      chain     FNFGW01L                           30
048300000427     C     *IN30         IFEQ      *ON
048400000427     C     FGWATB        ORNE      ' '
048500000427     C                   CLEAR                   FGWFF3
048600000427     C                   CLEAR                   FGWFF4
048700000427     C                   CLEAR                   FGWFL3
048800000427     C                   CLEAR                   FGWFL4
048900000612     C                   CLEAR                   FGWFGS
049000000427     C                   ENDIF
049100000427     C**
049200020807     C     K02FGV01      chain     FNFGV01L                           31
049300960319     C     *IN31         IFEQ      *OFF
049600960319     C*
049800020807     C                   Z-ADD     1             XX
049900020807     C                   MOVEL     VIDFFV        W003A
050000960319     C     W003A         LOOKUP    FFV(XX)                                30
050100010130     C     *IN30         IFEQ      *OFF
050200010130     C     VIDFFV        ANDNE     FGVLNA
050300010130     C                   MOVE      '1'           WLNABO
050400010130     C                   ENDIF
050600960319     C*
050700960319     C                   ENDIF
050800120213     c
055200950712     C*
055300900615     C                   ENDSR
055301130703      /free
055302130703       //-------------------------------------------------------------*
055303130703       //Reperimento Dati del job (Utente/Operativi)                  *
055304130703       //-------------------------------------------------------------*
055305130703       Begsr DatiJob;
055306130703
055307130703          in(e) §azute;
055308130703          if not %error;
055309130703           in(e) §datiute;
055310130703          endif;
055311130703          if %error or rsut = *blanks;
055312130703           tibs34r(tibs34ds);
055313130703           in §azute;
055314130703           in §datiute;
055315130703          endif;
055316130703
055317130703         endsr;
055318130703
055319130703      /end-free
058200940920     C**************************************************************************
058300940920     C*    CONTROLLO ESISTENZA SPUNTE PER LA BOLLA
058400940920     C**************************************************************************
058500900615     C     CTRNSC        BEGSR
058600940920     C*
058800120203     c                   clear                   uncollo
058900900831     C* CONTROLLO SE CTM NON PREVEDE LA SPUNTA DEI SEGNACOLLI:
059100900831     C                   MOVEL     '1B'          COD
059200900831     C                   MOVEL     *BLANKS       KEY
059300900831     C                   MOVEL     BLPCTM        KEY
059400940920     C     KTAB1         CHAIN     TABEL00F                           32
059500900831     C  N32              MOVEL     TBLUNI        DS1B
059600130703     C* NO SPUNTA: non elaboro
059700900831     C  N32§1BFG5        IFEQ      'N'
059900900831     C                   GOTO      ENDCTR
060000900831     C                   END
060100910827     C* SI SPUNTA: CONTROLLO SE ALMENO UN SEGNACOLLO SPUNTATO C'E'
060200940920     C     KBLT1         SETLL     FNBLT01L
060300130703     C     KBLT1         READE     FNBLT01L
060400910827     C*
060500130703     C                   DOW       not %eof(fnblt01l)
060600940921     C* LA FILIALE IN GESTIONE PER BORDERO' E' SIMFEL
060700940920     C                   Z-ADD     1             KNPG
060800940921     C                   Z-ADD     SIMFEL        KFGS
060900130703     C     KBRV1         SETLL     FNBRV01L
060901130703     c                   if        %equal(fnbrv01l)
060902130703     C                   eval      uncollo='S'
061000130703     C                   leavesr
061101130703     c                   endif
062700130703     C     KBLT1         READE     FNBLT000
062800130703     C                   ENDdo
063400900615     C*
063500900831     C     ENDCTR        ENDSR
063501130701     C**************************************************************************
063502130701     C*  STAMPA spedizioni spareggiate
063503130701     C**************************************************************************
063504130701     C     STAMPA        BEGSR
063505130701     c                   clear                   primo             1
063506130701     c     kblt1         setll     fnblt01l
063507130701     c     kblt1         reade     fnblt01l
063508130701    1c                   dow       not %eof(fnblt01l)
063509130701     C                   Z-ADD     1             KNPG
063510130701     C                   Z-ADD     SIMFEL        KFGS
063511130701     C     KBRV1         SETLL     FNBRV01L
063512130701     c* il collo non è stato spuntato in partenza sul foglio viaggio
063513130701    2c                   if        not %equal(fnbrv01l)
063514130701     c* Operazioni che faccio una volta sola per spedizione
063515130701   2ac                   if        primo=*blanks
063516130701     c                   z-add     blpaas        paas
063517130701     c                   z-add     blplnp        PLNP
063518130701     c                   z-add     blpnrs        PNRS
063519130701     c                   z-add     blpnsp        PNSP
063520130701     c* mese/giorno spedizione
063521130701     c                   clear                   gg
063522130701     c                   clear                   mm
063523130701     c                   move      blpmgs        gg                2 0
063524130701     c                   movel     blpmgs        mm                2 0
063525130701     c                   z-add     mm            pdats
063526130701     c                   movel     gg            pdats
063527130701     c                   z-add     blplna        PLNA
063528130701     c                   movel     blprsm        PRSM
063529130701     c                   z-add     blprmn        PRMN
063530130701     c                   z-add     blpncl        PNCL
063531130701     c                   z-add     blppkf        PPES
063532130701     c                   z-add     blpvlf        PVOL
063533130701     C                   move      BlpPdr        kpdpdr
063534130701     C                   move      'A'           kpdtip
063535130701     c                   clear                   descpr
063536130701     C     kfapd         chain     fiapd01l
063537130701    3c                   if        %found(fiapd01l)
063538130701     C                   MOVEL     APDRSC        DESCPR
063539130701    3c                   endif
063540130701     c                   eval      PPDR=%editc(blppdr:'X')+'-'+
063541130701     c                             descpr
063542130701     c                   clear                   gg
063543130701     c                   clear                   mm
063544130701     c                   clear                   pdrt
063545130701    3c                   if        blpdrt>0
063546130701     c                   eval      dataiso=%date(blpdrt:*iso)
063547130701     c                   eval      mm=%subdt(dataiso:*months)
063548130701     c                   eval      gg=%subdt(dataiso:*days)
063549130701     c                   z-add     mm            PDRT
063550130701     c                   movel     gg            pdrt
063551130701    3c                   endif
063552130701     c*
063553130701     c* dati seconda riga di stampa da stampare solo una volta
063554130701     c*
063555130701     c                   movel     blprsd        pdest
063556130701   2ac                   endif
063557130701
063558130701     c* Verifico se collo spuntato da qualche altra parte:
063559130701     c* cerco la spunta più recente del collo per la stampa
063560130701     c                   exsr      cerspu
063561130701     c*
063562130701     c* chi sono
063563130701     c                   clear                   pchisono
063564130701     c     kars          chain     fiars01l
063565130701     c                   if        %found(fiars01l)
063566130701     c                   eval      pchisono=arsnot
063567130701     c                   endif
063568130701     c                   z-add     bltnsc        PNSC
063569130701     c                   eval      PERR='MANCANTE'
063570130701    3c                   if        wtrov='S'
063571130701     c                   eval      PERR=%trim(perr) + ', SPUNTATO DA' +
063572130701     c                             ' ' + %editc(prtfgs:'X')
063574180119    4c***                if        prtspg<>*blanks
063575180119    4c                   if        prtnpg=3
063576130701     c                   eval      PERR=%trim(perr) + ' in I.M.' + prtspg
063577130701   x4c                   else
063578130701     C                   MOVEL     '7N'          COD
063579130701     C                   MOVEL(P)  PRTNPG        KEY
063580130701     C     KTAB1         CHAIN     TABEL00F
063581130701    5c                   if        %found(tabel00f)
063582130701     c                   eval      PERR=%trim(perr) + ' in ' +
063583130701     c                             %subst(tbluni: 1: 8)
063584130701    5c                   endif
063585130701    4c                   endif
063586130701     c                   eval      PERR=%trim(perr) + ' ' +
063587130701     c                             %editc(prtnfv:'Z') + ' il'
063588130701    4c                   if        prtdfv>0
063589130701     c                   eval      dataiso=%date(prtdfv:*iso)
063590130701     c                   eval      mm=%subdt(dataiso:*months)
063591130701     c                   eval      gg=%subdt(dataiso:*days)
063592130701     c                   eval      PERR=%trim(perr) + ' ' + %editc(gg:'X') +
063593130701     c                             '/' + %editc(mm:'X')
063594130701    4c                   endif
063595130701    3c                   endif
063596130701     c*
063597130701     c* stampa riga
063598130701     c* salto pagina
063599130701    3c                   if        $righe+2>60 and primo=*blanks
063600130701     c                   write     ls37t00
063601130701     c                   eval      $righe=7
063602130701    3c                   endif
063603130701    3c                   if        $righe+1>60 and primo<>*blanks
063604130701     c                   write     ls37t00
063605130701     c                   eval      $righe=7
063606130701    3c                   endif
063607130701     c                   if        primo=*blanks
063608130701     c                   write     ls37r01
063609130701     c                   add       1             $righe
063610130701     c                   eval      primo='N'
063611130701     c                   endif
063612130701     c* stampa righe successive alla prima
063613130701     c                   write     ls37r2
063614130701     c                   add       1             $righe
063615130701     c                   clear                   pdest
063616130701    2c                   endif
063617130701     c     kblt1         reade     fnblt01l
063618130701    1c                   enddo
063619130701     c                   endsr
105600120203     C**************************************************************************
105700120203     C* CERCA la spunta più recente del collo
105800120203     C**************************************************************************
105900120203     C     CERSPU        BEGSR
106000120203     C                   CLEAR                   WTROV
106100120203     c                   clear                   prtdfv
106200120203     c                   clear                   prtnfv
106300120203     c                   clear                   prtnpg
106400120203     c                   clear                   prtspg
106500120203     c*                  clear                   prtznc
106600120203     c                   clear                   prtfgs
106700120203     c                   clear                   wdfs
106800120203     c                   clear                   whms
106900120203     C     KBRV7         SETLL     FNBRV07L
107000120203     C     KBRV7         READE     FNBRV07L
107100120203     c                   dow       not %eof(fnbrv07l)
107200120203     c                   eval      wtrov='S'
107300120203     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLIo
107400120203     c                   clear                   fnlv53ds
107500120203     C                   z-add     brvnFV        D53NFV
107600120203     C                   MOVEL     brvNPG        D53NPG
107700120203     C                   MOVEL     brvFGS        D53FGS
107800120203     C                   CALL      'FNLV53R'
107900120203     C                   PARM                    fnlv53ds
108000120203     C* SE NON C'E' ERRORE MEMORIZZO LA DATA PIU' ALTA
108100120203     c* considero prima di tutto la data foglio più alta e
108200120203     c* a parità di data foglio prendo quella con data/ora immissione
108300120203     c* più alta
108400120203    4C     D53ERR        IFEQ      ' '
108500120203     c                   if        d53dfv>prtdfv or brvdfs>wdfs or
108600120203     c                             (brvdfs=wdfs and brvhms>whms)
108700120203     C                   MOVEL     D53DFV        PRTDFV
108800120203     C                   Z-ADD     D53NFV        PRTNFV
108900120203     C                   MOVEL     D53NPG        PRTNPG
109000120203     C                   MOVEL     D53SPG        PRTSPG
109100120203     C***                MOVEL     BRVZNC        PRTZNC
109200120203     C                   MOVEL     BRVFGS        PRTFGS
109300120203     c                   z-add     brvdfs        wdfs
109400120203     c                   z-add     brvhms        whms
109500120203     c                   endif
109600120203     c                   endif
109700120203     C     KBRV7         READE     FNBRV07L
109800120203     c                   enddo
109900120203     c                   endsr
