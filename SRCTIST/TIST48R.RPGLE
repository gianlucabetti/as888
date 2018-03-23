000100000000     H*------------------------------------------------------------------------*
000200060829     H* CONTROLLO ASSORBIMENTO X LNA - CREAZIONE FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000914     H*------------------------------------------------------------------------*
000600060829     FAZORG01L  IF   E           K DISK
000700060829     FTABEL00F  IF   E           K DISK
000800060829     FTNTBE01L  IF   E           K DISK
000900001016     FTITAS38C  IF   E           K DISK
001000060829     FWFALA01L  UF A E           K DISK    USROPN
001100000000     D*------------------------------------------------------------------------*
001200000000     D* SCHIERE
001300000000     D*------------------------------------------------------------------------*
001400000000     D*-------------------
001500060829     D* SCHIERE ORGANIGRAMMA CON RELATIVA CLASSIFICAZIONE TERRITORIALE GERARCHICA
001600000000     D*-------------------
001700000915     D FIL             S              3  0 DIM(500)                             *FILIALE
001800000915     D ARE             S              3  0 DIM(500)                             *AREA
001900000915     D DIV             S              1    DIM(500)                             *DIVISIONE
002000060829     D NTW             S              3    DIM(500)                             *NETWORK
002100060829     D*-------------------
002200060829     D* SCHIERA SCAGLIONI PESO / VOLUME DA TABELLA "ALA"
002300060829     D*-------------------
002400060829     D SPV             S                   DIM(200) LIKE(DALA)
002500060904     D SPVCOD          S              7    DIM(200)
002600000920     D*------------
002700000920     D* COMANDI
002800000920     D*------------
002900000920     D CMD             S             80    DIM(2) CTDATA PERRCD(1)
003000000000     D*-------------------
003100000918     D* SCHIERE FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA E TEMPI DI CONSEGNA
003200000914     D*-------------------
003300001025     D RBL             S              1    DIM(20)                              RAGGRUPPAMENTO BOLLA
003400000000     D*-------------------
003500000918     D* SCHIERE TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003600000000     D*-------------------
003700000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003800020226     D*-------------------
003900020226     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
004000020226     D*-------------------
004100020226     D OG143         E DS
004200060829     D*-------------------
004300060829     D* DS RIDEFINIZIONE FILE DATI
004400060829     D*-------------------
004500060829     D WFALADS_WRK   E DS                  extname(WFALA00F) prefix(k_)
004600060829     D WFALADS_SAV   E DS                  extname(WFALA00F)
004700000000     D*-------------------
004800000000     D* ARCHITETTURA
004900000000     D*-------------------
005000000000     D KPJBA         E DS
005100000000     D*-------------------
005200000000     D* DS TIPO BOLLA
005300000000     D*-------------------
005400000000     D DSTB          E DS
005500060829     D*-------------------
005600060829     D* DS SCAGLIONI PESO / VOLUME
005700060829     D*-------------------
005800060829     D DALA          E DS
005900000000     D*-------------------
006000000000     D* DS PARAMETRI DI LANCIO PGM
006100000000     D*-------------------
006200020226     D PARAM           DS
006300060829     D  PAROPE                        1
006400060829     D  PARDAI                        8  0
006500060829     D  PARDAF                        8  0
006600060829     D  PARDRI                        8  0
006700001016     D*-------------------
006800060829     D* DS CALCOLO INSTRADAMENTO
006900001016     D*-------------------
007000060829     D TISI95DS      E DS
007100000914     D*------------------------------------------------------------------------*
007200000918     D* VARIABILI DI WRK
007300000914     D*------------------------------------------------------------------------*
007400000914     D i               s              5i 0 inz(*zeros)
007500000915     D y               s              5i 0 inz(*zeros)
007600000914     D j               s              5i 0 inz(*zeros)
007700060829     D k               s              5i 0 inz(*zeros)
007800060829     D savSPV_P        s                   like(alaSPV) inz(*blanks)
007900060829     D savSPV_V        s                   like(alaSPV) inz(*blanks)
008000060829     D wNetwork        s              3    inz(*blanks)
008100000921     D*------------------
008200000921     D* DS "XSRDA8" - CONTROLLA DATA (8)
008300000921     D*------------------
008400000921     D WLBDA8          DS                  INZ
008500000921     D  G08DAT                 1      8  0
008600000921     D  G08INV                 9     16  0
008700000921     D  G08ERR                17     17
008800000921     D  G08TGI                18     22  0
008900000000     C*------------------------------------------------------------------------*
009000000000     C* MAIN LINE
009100000000     C*------------------------------------------------------------------------*
009200060829     C*
009300060829     C* Caricamento organigramma
009400060829     C                   exsr      carorg
009500060829     C*
009600060829     C* Caricamento tipi bolla
009700060829     C                   exsr      cartbl
009800060829     C*
009900060829     C* Caricamento scaglioni di peso / volume
010000060829     C                   exsr      carspv
010100000000     C*
010200060829     C* Elaborazione - creazione file
010300000920     C                   exsr      procedi
010400001109     C*
010500001109     C* Operazioni di chiusura
010600001109     C                   EXSR      chiude
010700000914     C*
010800000914     C                   SETON                                        LR
010900000914     C*------------------------------------------------------------------------*
011000000914     C* PROCEDI - Routine di elaborazione
011100000914     C*------------------------------------------------------------------------*
011200000914     C     PROCEDI       BEGSR
011300001106     C*
011400060829     C* CLRPFM del file dati
011500001016     C                   Z-ADD     80            LENGH            15 5
011600060829     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
011700000920     C                   PARM                    CMD(1)
011800000920     C                   PARM                    LENGH
011900000920     C*
012000000920     C* Se riuscito comando ==> procedo
012100001106     C                   if        not %error
012200000914     C*
012300060829     C* Apro il wrk-file (WFALA01L)
012400060829     C                   open      wfala01l
012500001016     C*
012600001016     C* Posizionemento e prima lettura
012700060829     C                   movel(p)  PARDAI        tasAAS
012800060829     C                   move(p)   PARDAI        tasMGS
012900001016     C*
013000060829     C     KEYtas38_p    setll     titas38c                           90        *FINE FILE
013100001016     C*
013200001016     C                   if        %found(titas38c)
013300060829     C                   read      titas38c
013400001016     C                   dow       not %eof(titas38c) and
013500060829     C                             tasAAS*10000+tasMGS <= PARDAF
013600060829     C*
013700060829     C* Verifico se la bolla corrente è da considerare ai fini dell'elaborazione
013800060829     C                   exsr      chkrec
013900001016     C*
014000060829     C* Se la bolla corrente è da considerare => elaboro
014100060829     C                   if        wRecOk = 'S'
014200001016     C                   exsr      creaa
014300001016     C                   endif
014400001016     C*
014500060829     C* Proseguo con la lettura x data delle bolle
014600001016     C                   read(e)   titas38c
014700001016     C                   enddo
014800001016     C                   endif
014900000914     C*
015000060829     C* Chiudo il wrk-file (WFALA01L)
015100060829     C                   close     wfala01l
015200000920     C                   endif
015300000920     C*
015400000914     C                   ENDSR
015500000914     C*------------------------------------------------------------------------*
015600060829     C* Routine di controllo validità record - creazione file dati
015700000914     C*------------------------------------------------------------------------*
015800060829     C     CHKREC        BEGSR
015900000914     C*
016000060829     C                   movel     'S'           wRecOk            1
016100060829     C*--------------------
016200060829     C* ESCLUDE LE BOLLE ESTERO
016300060829     C*--------------------
016400060829     C                   if        wRecOk = 'S'
016500060829     C                   if        tasNZD <> *blanks
016600060829     C                   movel     'N'           wRecOk
016700060829     C                   endif
016800060829     C                   endif
016900001016     C*--------------------
017000001025     C* ESCLUDE LE BOLLE NON VALIDE COME "RICAVO"
017100001016     C*--------------------
017200060829     C                   if        wRecOk = 'S'
017300001016     C                   z-add     1             j
017400060829     C     tasTBL        lookup    TB(j)                                  28
017500001025     C                   if        %found and RBL(j) = 'N'
017600001025     C                   else
017700060829     C                   movel     'N'           wRecOk
017800001025     C                   endif
017900001025     C                   endif
018000001030     C*
018100000914     C                   ENDSR
018200001113     C*------------------------------------------------------------------------*
018300060829     C* CREAA - CREA IL RECORD SUL FILE DATI IN RELAZIONE ALL' "ARRIVO"
018400001113     C*------------------------------------------------------------------------*
018500001113     C     CREAA         BEGSR
018600060829     C*
018700060829     C* Inizializzo la ds d wrk
018800060829     C                   clear                   wfalads_wrk
018900001113     C*
019000060829     C* Determino il network della bolla corrente
019100001113     C                   z-add     1             i
019200140404     C     tasLNA        lookup    fil(i)                                 55
019300001113     C                   if        %found
019400060829     C                   movel     ntw(i)        wNetwork
019500001113     C                   else
019600060829     C                   movel     *all'X'       wNetwork
019700001113     C                   endif
019800060829     C*
019900001113     C* Normalizzo la località mediante il TISI95
020000060829     C                   exsr      normloc
020100060829     C*
020200060829     C* Costruisco la chiave file dati con l'instradamento così normalizzato
020300060829     C                   movel     *blanks       k_alaNAZ                       * sempre solo Italia
020400060829     C                   movel     o95prv        k_alaPRV
020500060829     C                   movel     o95cap        k_alaCAP
020600060829     C                   movel     o95loc        k_alaLOC
020700060829     C                   z-add     o95lna        k_alaLNA
020800060829     C                   z-add     o95znc        k_alaZNC
020900060829     C*
021000060829     C* Determino in quale scaglione d peso / volume rientra la bolla
021100060829     C* (si considera sempre lo scaglione maggiore tra il peso e il volume)
021200060829     C                   clear                   savSPV_P
021300060829     C                   clear                   savSPV_V
021400060829     C                   z-add     1             k
021500060829     C                   dow       k <= %elem(SPV)
021600060829     C*
021700060829     C* Considero solo gli elementi valorizzati
021800060829     C                   if        SPV(k) <> *blanks
021900060829     C                   eval      DALA = SPV(k)
022000060829     C* Verifico lo scaglione d peso
022100061103     C                   if        tasPKF > §ALAPESDA AND tasPKF <= §ALAPESA
022200060829     C                   eval      savSPV_P = SPVCOD(k)
022300060829     C                   endif
022400060829     C* Verifico lo scaglione d volume
022500061103     C                   if        tasVLF > §ALAVOLDA AND tasVLF <= §ALAVOLA
022600060829     C                   eval      savSPV_V = SPVCOD(k)
022700060829     C                   endif
022800060829     C* Proseguo nella verifica dello scaglione successivo
022900060829     C                   add       1             k
023000060829     C                   else
023100060829     C                   leave
023200060829     C                   endif
023300060829     C                   enddo
023400060829     C*
023500060829     C* Dopo aver verificato tutti gli scaglioni tengo buono il maggiore tra peso e volume
023600060829     C                   if        savSPV_V > savSPV_P
023700060829     C                   eval      k_alaSPV = savSPV_V
023800060829     C                   else
023900060829     C                   eval      k_alaSPV = savSPV_P
024000060829     C                   endif
024100001113     C*
024200060829     C* Verifica se occorre inserire o aggiornare
024300060829     C     KEYala01_C    chain     wfala01l                           99
024400001113     C*
024500060829     C* Valorizzo il buffer del record
024600060829     C   99              clear                   wfala000
024700060829     C   99              exsr      insrec
024800060829     C  N99              exsr      addrec
024900060829     C   99              write     wfala000                                     *RECORD NON ESISTE
025000060829     C  N99              update    wfala000                                     *RECORD ESISTE
025100001113     C*
025200001113     C                   ENDSR
025300001113     C*------------------------------------------------------------------------*
025400001113     C* NORMLOC - Normalizza le località mediante il TISI95
025500001113     C*------------------------------------------------------------------------*
025600001113     C     NORMLOC       BEGSR
025700001113     C*
025800060829     C                   clear                   tisi95ds                       *azzera i parametri
025900001113     C                   movel     *blanks       i95tla                         *tipo lancio
026000001113     C                   movel     '7'           i95tcn                         *tipo controllo
026100060829     C                   movel     tasTSP        i95tsp                         *tipo servizio
026200060829     C                   movel     tasNZD        i95nar                         *nazione dest
026300060829     C                   movel     tasCAD        i95cap                         *cap dest
026400060829     C                   movel     tasLOD        i95loc                         *località dest
026500060829     C                   movel     tasPRD        i95prv                         *provincia dest
026600060829     C                   z-add     tasPKF        i95lkg                         * peso da fatturare
026700060829     C                   z-add     tasVLF        i95lmc                         * volume da fattur
026800060829     C                   movel     tasFFD        i95ffd                         * fermo deposito
026900060829     C                   movel     tasFTC        i95fi2                         * 1° cons. partic.
027000060829     C                   move      tasTC2        i95fi2                         * 2° cons. partic.
027100060829     C*
027200010308     C                   if        PARDRI > *zeros
027300010308     C                   z-add     PARDRI        i95dat                         *data di riferimento
027400001120     C                   else
027500060829     C                   eval      i95dat = tasAAS*10000+tasMGS                 *data di riferimento
027600020226     C                   endif
027700020226     C*
027800060829     C* Se la LNA è filiale FEDEX imposto flag opportuno
027900060829     C                   if        wNetwork = 'FED'
028000020226     C                   eval      i95fi1 = 'S'
028100020226     C                   endif
028200060829     C*
028300060829     C* Se la LNA è filiale DPD imposto flag opportuno
028400060829     C                   if        wNetwork = 'DPD'
028500060829     C                   eval      i95fi1 = 'D'
028600060829     C                   endif
028700020226     C*
028800001121     C                   call      'TISI95R'
028900060829     C                   parm                    tisi95ds
029000001113     C*
029100060829     C* Se nn reperito instradamento considero i dati originali della bolla
029200080414     C                   if        o95lna = *zeros
029300080414     C                   z-add     tasLNA        o95lna
029400080414     C                   z-add     tasZNC        o95znc
029500080414     C                   endif
029600080414     C                   if        o95lid = '3' or o95lid = '4'
029700080414     C                   else
029800060829     C                   movel     tasPRD        o95prv
029900060829     C                   movel     tasCAD        o95cap
030000060829     C                   movel     tasLOD        o95loc
030100001113     C                   endif
030200001113     C*
030300001113     C                   ENDSR
030400001113     C*------------------------------------------------------------------------*
030500060829     C* Inserisce la bolla corrente
030600001113     C*------------------------------------------------------------------------*
030700060829     C     INSREC        BEGSR
030800001113     C*
030900060829     C* Valorizzo i campi chiave precedentemente costituiti
031000060829     C                   eval      wfalads_sav = wfalads_wrk
031100001113     C*
031200001113     C* Valorizzo le date di lancio passate a video
031300060829     C                   z-add     PARDAI        alaDAI
031400060829     C                   z-add     PARDAF        alaDAF
031500001113     C*
031600060829     C* Valorizzo i dati relativi alla classificazione territoriale (rispetto alla LNA)
031700060829     C                   z-add     1             i
031800060829     C     alaLNA        lookup    fil(i)                                 55
031900060829     C                   if        %found
032000070118     C                   z-add     ARE(i)        alaARE
032100070118     C                   movel     DIV(i)        alaDIV
032200001113     C                   endif
032300001113     C*
032400001113     C* Valorizzo i dati relativi al contenuto della bolla (colli / peso / volume / spedizioni)
032500060829     C                   exsr      addrec
032600001113     C*
032700060829     C* Valorizzo il livello affidabilità nel calcolo dell'instradamento
032800060829     C                   movel     o95lia        alalia
032900001113     C*
033000001113     C                   ENDSR
033100060829     C*------------------------------------------------------------------------*
033200060829     C* Memorizza i dati della bolla (peso / volume / colli / spedizione)
033300060829     C*------------------------------------------------------------------------*
033400060829     C     ADDREC        BEGSR
033500060829     C*
033600060829     C                   add       1             alaTOS                         * tot spedizioni
033700060829     C                   add       tasNCL        alaTOC                         * tot colli
033800061103     C                   add       tasPKF        alaTOP                         * tot peso
033900061103     C                   add       tasVLF        alaTOV                         * tot volume
034000060829     C*
034100060829     C                   ENDSR
034200000914     C*------------------------------------------------------------------------*
034300000914     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
034400000914     C*------------------------------------------------------------------------*
034500000914     C     CARORG        BEGSR
034600000914     C*
034700060829     C* CARICO TUTTO L'ORGANIGRAMMA
034800060829     C     *LOVAL        SETLL     AZORG01L                           97        *FINE-ARCHIV
034900060829     C  N97              READ      AZORG01L                               97
035000000915     C                   Z-ADD     0             y                              *INDICE SCHIERA
035100060829     C     *IN97         DOWEQ     '0'                                          --- 1 -->
035200000914     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
035300000914     C     ORGFAG        ANDNE     'V'
035400000915     C                   ADD       1             y
035500000915     C                   Z-ADD     ORGFIL        FIL(y)
035600000915     C                   Z-ADD     ORGCAR        ARE(y)
035700000915     C                   MOVEL     ORGFL3        DIV(y)
035800000914     C                   MOVEL     ORGDE3        OG143
035900001016     C* CALCOLA NETWORK
036000060829     C                   MOVEL     §OGNTW        NTW(y)                         *NETWORK
036100001016     C                   ENDIF
036200060829     C                   READ      AZORG01L                               97
036300001016     C                   ENDDO
036400000914     C*
036500000914     C                   ENDSR
036600000000     C*------------------------------------------------------------------------*
036700000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
036800000000     C*------------------------------------------------------------------------*
036900000000     C     CARTBL        BEGSR
037000000000     C*--------------------
037100000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
037200000000     C*--------------------
037300000000     C                   Z-ADD     0             I
037400060829     C                   Z-ADD     1             TBLKUT
037500060829     C                   MOVEL     'TB'          TBLCOD
037600060829     C                   MOVEL     *BLANKS       TBLKEY
037700000000     C     KEYTAB        SETLL     TABEL00F
037800060829     C                   READ      TABEL00F                               98
037900060829     C     *IN98         DOWEQ     *OFF
038000000000     C     TBLCOD        ANDEQ     'TB'
038100000000     C     TBLFLG        IFEQ      *BLANKS
038200000000     C                   ADD       1             I
038300060829     C                   MOVEL(P)  TBLKEY        TB(I)                          *TIPO BOLLA
038400060829     C                   MOVEL(P)  TBLUNI        DSTB
038500060829     C                   MOVEL(P)  §TBRBL        RBL(I)
038600001016     C                   ENDIF
038700060829     C                   READ      TABEL00F                               98
038800001016     C                   ENDDO
038900000000     C*
039000000000     C                   ENDSR
039100060829     C*------------------------------------------------------------------------*
039200060829     C* CARSPV - CARICA GLI SCAGLIONI PESO / VOLUME DA TABELLA "ALA"
039300060829     C*------------------------------------------------------------------------*
039400060829     C     CARSPV        BEGSR
039500060829     C*
039600060829     C* Inizializzo le schiere d memorizzazione
039700060829     C                   clear                   SPV
039800060829     C                   clear                   SPVCOD
039900060829     C*
040000060829     C                   z-add     0             k
040100060829     C                   movel     'ALA'         tbeCOD
040200060829     C     tbeCOD        setll     tntbe01l
040300060829     C                   if        %equal(tntbe01l)
040400060829     C     tbeCOD        reade     tntbe01l
040500060829     C                   dow       not %eof(tntbe01l)
040600060829     C                   if        tbeATb = *blanks
040700060829     C                   add       1             k
040800060829     C                   eval      SPV(k) = tbeUNI
040900060829     C                   eval      SPVCOD(k) = tbeKE1
041000060829     C                   endif
041100060829     C     tbeCOD        reade     tntbe01l
041200060829     C                   enddo
041300060829     C                   endif
041400060829     C*
041500060829     C                   ENDSR
041600001109     C*--------------------------------------------------------------------------------------------*
041700001109     C* CHIUDE - Operazioni di chiusura
041800001109     C*--------------------------------------------------------------------------------------------*
041900001109     C     CHIUDE        BEGSR
042000001109     C*
042100060829     C* Calcolo instradamento
042200060829     C                   clear                   tisi95ds
042300001109     C                   movel     'C'           i95tla                         *TIPO LANCIO
042400001109     C                   call      'TISI95R'
042500060829     C                   parm                    tisi95ds
042600001109     C*
042700001109     C                   ENDSR
042800000000     C*------------------------------------------------------------------------*
042900000000     C* *INZSR - ROUTINE INIZIALE
043000000000     C*------------------------------------------------------------------------*
043100000000     C     *INZSR        BEGSR
043200000000     C*--------------------
043300000000     C* RICEVIMENTO PARAMETRI
043400000000     C*--------------------
043500000000     C     *ENTRY        PLIST
043600000000     C                   PARM                    KPJBA
043700010417     C                   MOVEL(p)  KPJBU         PARAM
043800000000     C*--------------------
043900000000     C* CHIAVI DI LETTURA
044000000000     C*--------------------
044100000000     C*
044200000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
044300000000     C     KEYTAB        KLIST
044400060829     C                   KFLD                    TBLKUT
044500060829     C                   KFLD                    TBLCOD
044600060829     C                   KFLD                    TBLKEY
044700000914     C*
044800060829     C* CHIAVE LETTURA TIASS01L - COMPLETA
044900060829     C     KEYala01_c    KLIST
045000060829     C                   KFLD                    k_alaNAZ
045100060829     C                   KFLD                    k_alaPRV
045200060829     C                   KFLD                    k_alaCAP
045300060829     C                   KFLD                    k_alaLOC
045400060829     C                   KFLD                    k_alaLNA
045500060829     C                   KFLD                    k_alaZNC
045600060829     C                   KFLD                    k_alaSPV
045700001016     C*
045800060829     C* CHIAVE LETTURA TITAS38C - PARZIALE
045900060829     C     KEYtas38_p    KLIST
046000060829     C                   KFLD                    tasAAS                         -anno
046100060829     C                   KFLD                    tasMGS                         -mese/giorno
046200000000     C*
046300000000     C                   ENDSR
046400000914
046500000920** CMD - COMANDI CL
046600060829CLRPFM FILE(WFALA00F)
046700000920
