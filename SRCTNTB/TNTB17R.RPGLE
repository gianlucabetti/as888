000100011108      *--------------------------------------------------------------------------------------------*
000200011108      * Gestione tabella Limiti C/Assegno 'LCA'                                                    *
000300011108      *--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500011108      *--------------------------------------------------------------------------------------------*
000600011108      * Data base                                                                                  *
000700011108      *--------------------------------------------------------------------------------------------*
000800011108     FTNTB17D   CF   E             WORKSTN
000900011108     FTNTBE01L  UF A E           K DISK
001000120410     Fazorg01l  iF A E           K DISK
001001130806     Fcnaco00f  iF   E           K DISK
001100011108      *--------------------------------------------------------------------------------------------*
001200011108      * Data structure                                                                             *
001300011108      *--------------------------------------------------------------------------------------------*
001400011108      * Schiere a tempo di compilazione
001500011108      *---
001600120413     D Opz             S             37    DIM(4) CTDATA PERRCD(1)              *Messaggi video
001700130807     D Msg             S             78    DIM(7) CTDATA PERRCD(1)              *Messaggi video
001800011108      *---
001900011108      * Schiere memorizzazione dati correnti
002000011108      *---
002100011108     D DLca          E DS                                                       *reperimento dati
002200050301     D  alflcab              103    115
002300011108      *---
002400011106     D Tibs02Ds      E DS
002401130806      *---
002402130806     D TIBS69DS      E DS
002403130806     D ds_cnaco      E DS                  extname(CNACO00F) prefix('DS_')
002404130806     D ds_cnind      E DS                  extname(CNIND00F)
002405130806     D ds_cnclp      E DS                  extname(CNCLP00F)
002406130806     D ds_fncls      E DS                  extname(FNCLS00F)
002500011106      *----------
002600011108      * Variabili
002700010925      *----------
002800011108     D $Fine           S              1    INZ('N')                             *fine programma
002900011108     D TipVid          S              1    INZ('1')                             *video in gestione
003000011108     D $CarVd1         S              1    INZ('1')                             *video in gestione
003100011108     D $CarVd2         S              1    INZ('0')                             *video in gestione
003200011108     D Win             S             99A
003300120413     D Wsilna          S              1A
003301130806     D livello         S              1  0
003400120410     D wlna            S              3  0 INZ
003401130806     D kkut            S                   LIKE(acokut) inz(1)
003402130806     D kksc            S                   LIKE(acoksc)
003403130806     D codut           S              1  0 inz(1)
003500120410      * - Parametri per TNSD24R
003600120410     d §§Cod           s              3    inz
003700120410     d §§Tip           s                   like(ORGfag)  inz
003800120410     d §§Des           s             25    inz
003900120410     d §§Pra           s              3    inz
004000011108      *---
004100011108      * Indici
004200011108      *---
004300011108     D X               S              2  0 INZ                                  *indice
004400011108     D Y               S              2  0 INZ                                  *indice
004500011108      *---
004600011108      * Architettura
004700011108      *---
004800011108     D Kpjba         E DS                                                       *architettura
004900020715
005000020715     d DdatiUte      e ds
005100020715     d AzuteDs       e ds                  ExtName(Azute00f)
005200020715     d Tibs34Ds      e ds                  inz
005300011108      *--------------------------------------------------------------------------------------------*
005400011108      * Main lines
005500011108      *--------------------------------------------------------------------------------------------*
005600011108      *
005700011108      * Operazioni iniziali
005800011106     C                   EXSR      RutInz
005900011108      *
006000011108      * Gestione video
006100011106     C     $Fine         DOUEQ     'S'
006200011106     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile vuoto
006300011106     C     TipVid        CASEQ     '2'           GesVd2                          *Sfile vuoto
006400011105     C                   ENDCS
006500011105     C                   ENDDO
006600011108      *
006700000000     C                   SETON                                        LR
006800011108      *--------------------------------------------------------------------------------------------*
006900011108      * GesVd1 - Gestione videata selezione codice tabella -
007000011108      *--------------------------------------------------------------------------------------------*
007100011106     C     GesVd1        BEGSR
007200010925      *
007300011108      * Inizializzazione videata
007400011106     C     $CarVd1       ifeq      *on
007500011106     C                   EXSR      CarVd1
007600011106     C                   move      *off          $CarVd1
007700010925     C                   endif
007800120413     c                   clear                   v2pzd
007900011106      * Scrivo la testata
008000011108     C                   WRITE     Tb17T01
008100011106      *
008200011108      * Se esistono errori sulla videata
008300011106      * emetto la write del formato a indicatori spenti per vedere
008400011106      * le eventuali decodifiche
008500011106     C     *IN25         ifeq      *on
008600011106     C                   movea     *In           Win
008700011106     C                   move      *ALL'0'       in3040           11
008800011106     C                   movea     IN3040        *in(40)
008900011108     C                   WRITE     Tb17V01
009000011106     C                   movea     Win           *In
009100010925     C                   endif
009200011106      *
009300011108     C                   EXFMT     Tb17V01
009400120410     c                   setoff                                       902540
009500130806     c                   setoff                                       415042
009600011002      *
009700011106     c     '?'           scan      V1Cca                                  50
009800011105      * Ricerca
009900011105     c                   select
010000011105      * F3=Fine
010100011106     C     *inKc         wheneq    *on
010200010925     C                   EXSR      F03Ges
010300011105      *
010400011106     C     *in50         wheneq    *on
010500011106     C                   EXSR      Search
010501130806     C     *inkg         wheneq    *on
010502130806     C                   seton                                        03
010600011105     C                   other
010700010925      *
010800010925      * Controlli e decodifiche
010900010925     C                   EXSR      CtrD1
011000011105      *
011100120410     C     *IN90         ifeq      *off
011200010925      *
011300011108      * Gestisco il video di dettaglio
011400011105     C                   move      *on           $carvd2
011500011106     C                   move      '2'           TipVid
011600011105     C                   endif
011700011105      *
011800011105     C                   endsl
011900011105      *
012000011105      *
012100010925     C                   endsr
012200011108      *--------------------------------------------------------------------------------------------*
012300011108      * GesVd2 - Gestione videata dettaglio dati
012400011108      *--------------------------------------------------------------------------------------------*
012500011106     C     GesVd2        BEGSR
012600011105      *
012700011108      * Inizializzazione videata
012800011106     C     $CarVd2       ifeq      *on
012900011106     C                   EXSR      Carvd2
013000011106     C                   move      *off          $CarVd2
013100011105     C                   endif
013200011108      * Scrivo la testata
013300011108     C                   WRITE     Tb17T01
013400011105      *
013500011106      * Se esistono errori sulla videata
013600011105      * emetto la write del formato a indicatori spenti per vedere
013700011105      * le eventuali decodifiche
013800011106     C     *IN25         ifeq      *on
013900011106     C                   movea     *In           Win
014000011106     C                   move      *ALL'0'       in3040           11
014100011106     C                   movea     IN3040        *in(40)
014200011108     C                   WRITE     Tb17V02
014300011106     C                   movea     Win           *In
014400011106     C                   endif
014500011105      *
014600011106     C     *in04         ifeq      *on
014700011108     C                   WRITE     Tb17V02
014800011106     C                   EXFMT     PROTECT
014900011106      *
015000011106     C                   else
015100011108     C                   EXFMT     Tb17V02
015200011106     C                   endif
015300011108      *
015400011108     C                   Select
015500011108      * F3=Fine
015600011106     C     *inKc         wheneq    *on
015700011105     C                   EXSR      F03Ges
015800011108      * F12=ritorno
015900011106     C     *inKl         wheneq    *on
016000011105     C                   EXSR      F12Ges
016100011108      *
016200011108     C                   other
016300011105      *
016400011105      * Controlli e decodifiche
016500011105     C                   EXSR      CtrD2
016600011106      *
016700011106      * Non ci sono errori e ho chiesto conferma/annullamentoripristino
016800011106     c     *in25         ifeq      *off
016900011106     c     *inKf         ifeq      *on
017000011106     c     *inKe         oreq      *on
017100011106     c     *inKq         oreq      *on
017200011105      *
017300011106      * Eseguo aggiornamento
017400011105     C                   EXSR      AggTnt
017500011105     C                   endif
017600011106     C                   endif
017700011108      *
017800011105     C                   endsl
017900011105     C                   endsr
018000011108      *--------------------------------------------------------------------------------------------*
018100011108      * Search - Ricerca su campi video
018200011108      *--------------------------------------------------------------------------------------------*
018300130806     C     Search        BEGSR
018400010928      *
018500011106     C                   move      *blanks       V1CcaD
018600011106     C                   move      *blanks       Tibs02Ds
018700011106     c                   move      'LCA'         T02Cod
018800011106     c                   move      'R'           T02Mod
018900011106     c                   movel     KnSif         T02Sif
019000011105     C                   CALL      'TIBS02R'
019100011106     C                   PARM                    kpjba
019200011106     C                   PARM                    Tibs02Ds
019300011106     C                   if        T02Err =  *blanks
019400011106     C                   movel     T02ke1        V1Cca
019500120413     C                   movel     T02ke2        V1Clna
019501130806     C                   move      T02ke2        V1Cksc
019600011106     C                   movel     T02uni        v1CcaD
019700011106     c                   endif
019800120413     c
019900120413     c                   if        v1clna>*zeros
020000120413     C                   MOVEL     v1clna        wlna
020100120413     c     wlna          chain     azorg01l
020200120413     c                   if        %found(azorg01l)
020300120413     c                   movel(p)  orgdes        v1dlna
020400120413     c                   endif
020500120413     c                   endif
020501130806     c                   if        v1cksc>0
020506130806     C                   clear                   TIBS69DS
020507130806     C                   move      v1cksc        I69kac
020508130806     C                   call      'TIBS69R'
020509130806     C                   parm                    tibs69DS
020510130806     C                   parm                    ds_cnaco
020511130806     C                   parm                    ds_cnind
020512130806     C                   parm                    ds_cnclp
020513130806     C                   parm                    ds_fncls
020514130806     c                   eval      v1dksc=DS_acorag
020516130806     c                   endif
020600010928      *
020700010925     C                   ENDSR
020800011108      *--------------------------------------------------------------------------------------------*
020900011108      * F03Ges - Tasto funzionale F03 -> Fine programma
021000011108      *--------------------------------------------------------------------------------------------*
021100011106     C     F03Ges        BEGSR
021200011108      *
021300011106     C                   movel     'S'           $Fine                          *fine pgm
021400011108      *
021500010925     C                   ENDSR
021600011108      *--------------------------------------------------------------------------------------------*
021700011108      * F12Ges - Tasto funzionale f12 -> Ritorno
021800011108      *--------------------------------------------------------------------------------------------*
021900011106     C     F12Ges        BEGSR
022000011108      *
022100011106     C                   eval      TipVid = '1'
022200011108      *
022300010925     C                   ENDSR
022400011108      *--------------------------------------------------------------------------------------------*
022500011108      * CarVd1 - Carica primo video
022600011108      *--------------------------------------------------------------------------------------------*
022700011106     C     CarVd1        BEGSR
022800011108      *
022900120410     c                   setoff                                       2590
023000011106     c                   setoff                                       010204
023100011108     C                   clear                   Tb17V01
023200011106     C                   move      '  ?'         V1Cca
023300130807     c****               eval      v1dlna='"   " = Limiti STANDARD '
023301130807     c                   eval      v1dlna='"999" = Tutte           '
023400120413     c                   clear                   v1clna
023401130806     c                   clear                   v1cksc
023402130806     c                   clear                   v1dksc
023500011106      *
023600000000     C                   ENDSR
023700011108      *--------------------------------------------------------------------------------------------*
023800011108      * CarVd2 - Carica secondo video
023900011108      *--------------------------------------------------------------------------------------------*
024000011106     C     CarVd2        BEGSR
024100011106     C                   setoff                                       010204
024200130806     C                   setoff                                       050607
024300120413     C                   setoff                                       3233
024400011106      *
024500011106      * Aggancio la tabella, se trovo il codice sono in modifica
024600011108      * o ripristino (se record annullato), altrimenti in immissione
024700011106     c                   movel     V1Cca         CodTab            3
024701130806     c* imposto la key2 per la chain su tntbe
024703130806    1c                   if        v1cksc>0
024704130806    2c                   if        v1clna>*zeros and v1clna<>'999'
024705130806     c                   eval      livello=4
024706130806     c                   movel(p)  v1clna        tbeke2
024707130806     c                   move      v1cksc        tbeke2
024708130806   x2c                   else
024709130806     c                   eval      v2clna='999'
024710130806     c                   eval      livello=3
024711130806     c                   movel(p)  '999'         tbeke2
024712130806     c                   move      v1cksc        tbeke2
024713130806    2c                   endif
024714130806   x1c                   else
024715130806    2c                   if        v1clna>*zeros    and v1clna<>'999'
024716130806     c                   eval      livello=2
024717130806     c                   movel(p)  v1clna        tbeke2
024718130806   x2c                   else
024720130806     c                   eval      livello=1
024721130806     c                   clear                   tbeke2
024722130806    2c                   endif
024723130806    1c                   endif
024800120413     c                   eval      wSIlna='S'
024900011106     c                   EXSR      ChnTnt
025000120413     c
025100011106      *
025200011106      * MODIFICA/RIPRISTINO
025300120413    1c  n21              do
025400011106     c                   seton                                        02
025500011106     c                   movel     Tbeuni        DLca
025600120410     c                   eval      v2Cca =V1Cca
025700011106     c                   eval      v2CcaD=§lcads
025800011106     c                   z-add     §LcaMn        V2LMn
025900011106     c                   z-add     §LcaX1        V2LX1
026000011106     c                   z-add     §LcaX2        V2LX2
026100011106     c                   z-add     §LcaCo        V2LCo
026200120413    2c                   if        alflcab>=*zeros
026300050301     c                   z-add     §Lcab         V2Lb
026400050301     c                   else
026500050301     c                   movel     *hival        v2lb
026600120413    2c                   endif
026700011106      *
026800120413    2C     TbeAtb        ifeq      *blanks
026900011106     C                   setoff                                       04
027000011106     C                   eval      V2PzD = Opz(2)
027100011106     C                   else
027200011106     C                   seton                                        04
027300011106     C                   eval      V2PzD = Opz(3)
027400120413    2C                   endif
027500120413    1c                   enddo
027600011106      *
027700011106      * IMMISSIONE
027800120413    1c   21              do
027900011106     c                   movel     *blanks       v1ccad
028000011106     c                   clear                   Tb17V02
028100011106     c                   movel     V1Cca         V2Cca
028200050301     c                   movel     *hival        V2lb
028300011106     c                   seton                                        01
028400011106     C                   eval      V2PzD = Opz(1)
028500120413     c
028600130806     c* Se presente LNA o cliente, prendo i dati da record STD
028601130806     c*
028700130806    2c                   if        livello>1
028800120413     c                   eval      wSIlna='N'
028801130807     c****R              dow       *in21=*on and livello>1
028802130807     c***R               sub       1             livello
028803130806     c* se devo provare il livello 2 ma non ho la linea di arrivo salto il liv 2
028804130807     c****R              if        livello=2 and
028805130807     c***R                         (v1clna=*zeros or v1clna='999')
028806130807     c****R              sub       1             livello
028807130807     c***R               endif
028808130807     c****R              select
028809130807     c****R              when      livello=1
028810130807     c****R              clear                   tbeke2
028811130807     c****R              when      livello=2
028812130807     c****R              movel(p)  v1clna        tbeke2
028813130807     c****R              when      livello=3
028814130807     c****R              movel     '999'         tbeke2
028815130807     c***R               endsl
028816130807     c****R              exsr      chntnt
028817130807     c****R              enddo
028818130807     c                   clear                   tbeke2
028900130807     c                   EXSR      ChnTnt
029000120413     c
029100120413    3c                   if        not *in21
029200120413     c                   seton                                        06
029300120413     C                   eval      V2PzD = Opz(4)
029400120413     c                   movel     Tbeuni        DLca
029500120413     c                   eval      v2Cca =V1Cca
029600120413     c                   eval      v2CcaD=§lcads
029700120413     c                   z-add     §LcaMn        V2LMn
029800120413     c                   z-add     §LcaX1        V2LX1
029900120413     c                   z-add     §LcaX2        V2LX2
030000120413     c                   z-add     §LcaCo        V2LCo
030100120413    4c                   if        alflcab>=*zeros
030200120413     c                   z-add     §Lcab         V2Lb
030300120413     c                   else
030400120413     c                   movel     *hival        v2lb
030500120413    4c                   endif
030600120413     c
030700120413    3c                   endif
030800120413    3c                   endif
030900120413     c                   seton                                        21
031000120413    1c                   enddo
031100011106     c
031200120413     c                   clear                   v2clna
031300120413    1c                   if        v1clna>*zeros
031400120413     c                   eval      v2Clna=V1Clna
031500120413     c                   seton                                        05
031600120413     c                   eval      v2dlna=V1dlna
031700120413    1c                   else
031800130807     c                   eval      v2dlna='LIMITI STANDARD'
031900120410     C                   ENDIF
031901130806     c                   clear                   v2cksc
031902130806     c                   if        v1cksc>0
031903130806     c                   eval      v2cksc=v1cksc
031904130807     c******             if        v1clna=*zeros  or v1clna=*blanks
031905130807     c*****              eval      v2clna='999'
031906130807     c******             clear                   v2dlna
031907130807     c*****              endif
031908130806     c                   seton                                        07
031909130806     c                   eval      v2dksc=v1dksc
031910130806     c                   endif
032000120410     C
032100011105     C                   ENDSR
032200011108      ************************************************************
032300011108      * Controllo e decodifica prima videata
032400011108      ************************************************************
032500010925     C     CtrD1         BEGSR
032600011106      *
032700011106     c                   clear                   V1Msg
032800120410     c                   clear                   v1dlna
032900011106      *
033000011106      * Codice tabella obbligatorio
033100011106     c     V1Cca         ifeq      *blanks
033200120410     c                   seton                                        254090
033300011106     c                   eval      V1Msg = Msg(1)
033400120410     c                   leavesr
033500011106      *
033600011106      * Se esiste decodifico
033700011106     c                   else
033800011106     C                   clear                   tibs02ds
033900011106     C                   movel     'C'           t02mod
034000011106     C                   movel     Sif           t02sif
034100011106     C                   movel     'LCA'         t02cod
034200011106     C                   movel     V1Cca         t02ke1
034300011106     C                   call      'TIBS02R'
034400011106     C                   parm                    KPJBA
034500011106     C                   parm                    TIBS02DS
034600011106     C                   if        t02err =  *blanks
034700011106     C                   movel     t02uni        V1CcaD
034800011106     c                   endif
034900011106     c                   endif
035000010925      *
035100120410      * controllo eventuale LNA immessa
035200120410      *
035300120410     c     '?'           scan      V1Clna                                 90
035400120410      * Ricerca
035500120410     c                   if        *in90
035600120410     c                   clear                   §§cod
035700120410     c                   clear                   §§tip
035800120410     c                   clear                   §§des
035900120410     c                   clear                   §§pra
036000120410     C                   CALL      'TNSD24R'
036100120410     C                   PARM                    §§Cod
036200120410     C                   PARM                    §§Tip
036300120410     c
036400120410     C                   PARM                    §§DES
036500120410     C                   PARM                    §§PRA
036600120410     c                   movel     §§cod         v1clna
036700120410     c                   movel     §§des         v1dlna
036800120410     c                   leavesr
036900120410     c                   endif
037000120410     c
037100120410     c                   if        v1clna<>*blanks and v1clna<>'000'
037101130807     c                             and v1clna<>'999'
037200120410     C                   TESTN                   v1clna               36
037300120410     c                   move      v1clna        w001a             1
037400120410     c
037500120410     c                   if        not *in36 or w001a<'0'
037600120410     c                   seton                                        254190
037700120410     c                   eval      V1Msg = Msg(5)
037800120410     c                   leavesr
037900120410     c                   endif
038000120410
038100120410     C                   MOVEL     v1clna        wlna
038200120410     c     wlna          chain     azorg01l
038300120410     c                   if        not %found(azorg01l) or orgfva<>*blanks or
038400120410     c                             (orgfag <>'F' and orgfag<>'A')
038500120410     c                   seton                                        254190
038600120410     c                   eval      V1Msg = Msg(5)
038700120410     c                   leavesr
038800120410     c                   else
038900120410     c                   movel     orgdes        v1dlna
039000120410     c                   endif
039100120410     c                   endif
039200120413     c
039300130807     c                   if        v1clna<=*zeros OR V1CLNA='999'
039400130807     c*****              eval      v1dlna='"   " = Limiti STANDARD '
039401130807     c                   eval      v1dlna='"999" = Tutte           '
039500120413     c                   endif
039501130806
039502130806     c* controllo eventuale cliente immesso
039504130806      * Ricerca
039505130806     c                   if        *in03 and v1cksc=0 and v1dksc<>*blanks
039506130806     C                   MOVEL     rsut          PARDUT           30
039507130806     C                   MOVEL     *BLANKS       DESCDS           48
039508130807     C                   MOVEL     v1dksc        DESCDS
039509130806     C* PARSTA=9 ESCLUDO ANNULLATI
039510130806     C                   Z-ADD     9             PARSTA
039511130806     C                   Z-ADD     dutkci        CCC               4 0
039512130806     C                   CLEAR                   PARFLR
039513130806     C                   CLEAR                   PARNUM
039514130806     C                   CLEAR                   PARKCM
039515130806     C                   CLEAR                   PARKSM
039516130806     C                   CLEAR                   PARKDM
039517130806     C                   CLEAR                   PARflr
039518130806     C                   CLEAR                   PARDIT
039519130806     C                   CALL      'XALFA3BR'
039520130806     C                   PARM                    PARDUT
039521130806     C                   PARM                    CODUT
039522130806     C                   PARM                    DESCDS
039523130806     C                   PARM                    CCC
039524130806     C                   PARM                    PARSTA            1 0
039525130806     C                   PARM                    PARFLR           90
039526130806     C                   PARM                    PARDIT            3
039527130806     C                   PARM                    PARNUM            2 0
039528130806     C                   PARM                    PARKCM           80
039529130806     C                   PARM                    PARKSM          140
039530130806     C                   PARM                    PARKDM           60
039531130806     C                   PARM      'S'           PAResci           1
039532130806     C                   PARM      '  '          PARERR            2
039533130806    2C     PARSTA        IFGT      -1
039534130806     C                   MOVEL     paRksm        v1cksc
039535130806     C                   MOVEL     descds        v1dksc
039536130806     c                   else
039537130806     c                   clear                   v1cksc
039538130806     c                   endif
039539130807     c                   seton                                        90
039540130806     c                   leavesr
039541130806     c                   endif
039542130806     c                   if        v1cksc<>0
039550130806
039551130806     C                   MOVEL     v1cksc        kksc
039552130806     c     kaco          chain     cnaco00f
039553130806     c                   if        %found(cnaco00f)
039554130807     c                   movel     acorag        v1dksc
039555130806     c                   else
039556130806     c                   seton                                        254290
039557130806     c                   eval      V1Msg = Msg(6)
039558130806     c                   leavesr
039560130806     c                   endif
039561130806     c                   endif
039562130807     c* se immesso il cliente la lna deve esserci o essere = 999
039563130807     c                   if        v1cksc>*zeros
039564130807     c                             and v1clna =*blanks
039565130807     c                              or v1clna = '000'
039566130807     c                   seton                                        254190
039567130807     c                   eval      V1Msg = Msg(7)
039568130807     c                   leavesr
039569130807     c                   endif
039600120410     c
039700010925     C                   ENDSR
039800011108      ************************************************************
039900011108      * Controllo e decodifica seconda videata
040000011108      ************************************************************
040100011105     C     CtrD2         BEGSR
040200011108     C                   setoff                                       2531
040300011106     C                   setoff                                       3233
040400011106     C                   clear                   V2Msg
040500011106     C
040600011106      *
040700011106      * Controlli da effettuarsi se record non annullato
0408000111061a   C     *In04         ifeq      *off
040900011106      *
041000011106      * Controllo che l'importo max 1 non sia maggiore del max2
0411000111062a   C     V2LX1         ifgt      V2LX2
041200011106     C                   seton                                        2533
041300011108     C                   eval      V2Msg = Msg(4)
0414000111062-   C                   endif
041500011106      *
041600011108      * Se ho digitato uno dei due importi massimi
041700011108      * controllo che l'importo minimo non sia maggiore dei due massimi
0418000111082a   C     V2LX1         ifgt      0
041900011108     C     V2LX2         orgt      0
042000011108      *
0421000111083a   C     V2LMn         ifgt      V2LX1
0422000111083a   C     V2LX1         andgt     0
042300011106     C     V2LMn         orgt      V2LX2
0424000111083a   C     V2LX2         andgt     0
042500011106     C                   seton                                        2532
042600011108     C                   eval      V2Msg = Msg(3)
0427000111083-   C                   endif
0428000111082-   C                   endif
042900011106      *
043000011106      * Descrizione non inserita
0431000111062a   C     V2CcaD        ifeq      *blanks
043200011106     C                   seton                                        2531
043300011108     C                   eval      V2Msg = Msg(2)
0434000111062-   C                   endif
043500011106      *
0436000111061-   C                   endif
043700011105      *
043800011105     C                   ENDSR
043900011108      ************************************************************
044000011108      * Aggiornamento tabella LCA
044100011108      ************************************************************
044200010925     C     AggTnt        BEGSR
044300010926      *
044400011105     C                   SELECT
044500011106      * F06=Conferma
044600011106     C     *InKf         wheneq    *on
044700011106      *
044800011105      * Immissione
044900011108     C     *in01         ifeq      *on
045000011105     C                   EXSR      RieTnt
045100011106     C                   move      *zeros        TbeFlt
045200011107     C                   move      'S'           TbeFtt
045300011108     c                   move      *zeros        Tbedtr
045400011105     C                   WRITE     tntbe000                             22
045500011106     C                   endif
045600011105      * Modifica
045700011106     C     *in02         Ifeq      *on
045800011106     C                   EXSR      RieTnt
045900011106     C                   UPDATE    tntbe000
046000011106     C                   endif
046100011105      *
046200011106      * F16=Annullamento
046300011105     C     *inkq         wheneq    *on
046400011106     C     *in04         andeq     *off
046500011105     C                   move      'A'           tbeatb
046600011107     C                   move      *blanks       tbeftr
046700011106     C                   UPDATE    tntbe000
046800011105      *
046900011106      * F5=Ripristino
047000011105     C     *inke         wheneq    *on
047100011106     C     *in04         andeq     *on
047200011105     C                   move      ' '           tbeatb
047300011107     C                   move      *blanks       tbeftr
047400011106     C                   UPDATE    tntbe000
047500011105      *
047600011105     C                   ENDSL
047700011108      * Torno alla prima videata che carico come da inizio pgm
047800011106     C                   move      '1'           TipVid
047900011106     C                   move      *on           $CarVd1
048000011106     C
048100010925     C                   ENDSR
048200011108      ************************************************************
048300011108      * Riempimento file tabella
048400011108      ************************************************************
048500010925     C     RieTnt        BEGSR
048600011106      *
048700011106      * Codice tabella
048800011106     c                   movel     *blanks       TbeAtb
048900011106     c                   move      Sif           TbeSif
049000011106     c                   move      'GE'          TbeApl
049100011106     c                   movel     'LCA'         TbeCod
049200011106     c                   movel     V2cca         TbeKe1
049300011106     c                   movel     *blanks       TbeKe2
049400120413     c                   if        v1clna>*zeros
049500120413     c                   movel     v1clna        tbeke2
049600120413     c                   endif
049601130806     c                   if        v1cksc>*zeros
049602130806     c                   move      v1cksc        tbeke2
049603130807     c*****              if        v1clna<=*zeros or v1clna='999'
049604130807     c*****              movel     '999'         tbeke2
049605130807     c*****              endif
049606130806     c                   endif
049700011105      *
049800011106     c                   clear                   DLca
049900011108     c                   movel     V2CcaD        §LCADS
050000011108     c                   z-add     V2Lmn         §LCAMN
050100011108     c                   z-add     V2Lx1         §LCAX1
050200011108     c                   z-add     V2Lx2         §LCAX2
050300011108     c                   z-add     V2Lco         §LCACO
050400050301     c                   z-add     V2Lb          §LCAb
050500011108     c                   movel     DLca          TbeUni
050600011106      *
050700011106     c                   move      *blanks       TbeFtr
050800010925     C
050900010925     C                   ENDSR
051000011108      *--------------------------------------------------------------------------------------------*
051100011108      * Aggancio tabella
051200011108      *--------------------------------------------------------------------------------------------*
051300011106     c     ChnTnt        BEGSR
051400011106     c                   move      'LCA'         TbeCod
051500011106     c                   clear                   TbeKe1
051600011106     c                   movel     CodTab        TbeKe1
051700130806     c****               move      *blanks       TbeKe2
051800120413     c* Se richiesta la chain con la linea di arrivo
051900130806     c***                if        wSIlna='S'
052000130806     c***                if        v1clna >*zeros
052100130806     c***                movel     v1clna        tbeke2
052200130806     c***                endif
052300130806     c***                endif
052400120410     c
052500011106     c                   move      *blanks       TbeLin
052600011106     c                   movel     Knsif         TbeSif
052700120413     c                   if        wsilna='S'
052800011106     c     k01tbe5       chain     tntbe01l                           21
052900120413     c                   else
053000120413     c     k01tbe5       chain(n)  tntbe01l                           21
053100120413     c                   endif
053200011106      * Se non ho trovato il record con il sistema informativo
053300011106      * che ho in linea, lo abblenco
053400011106     c   21              do
053500011106     c                   eval      TbeSif= *blanks
053600011106     c                   eval      Sif= *blanks
053700120413     c                   if        wsilna='S'
053800011106     c     k01tbe5       chain     tntbe01l                           21
053900120413     c                   else
054000120413     c     k01tbe5       chain(n)  tntbe01l                           21
054100120413     c                   endif
054200011106     c  N21              eval      Sif= *blanks
054300011106     c                   enddo
054400011108      *
054500011106     c                   ENDSR
054600011108      *--------------------------------------------------------------------------------------------*
054700011108      * RutInz - operazioni iniziali
054800011108      *--------------------------------------------------------------------------------------------*
054900011108     C     RutInz        BEGSR
055000011108      *
055100011108      * Ricevimento parametri
055200000000     C     *ENTRY        PLIST
055300000000     C                   PARM                    kpjba
055400011108      *
055500011108      * Chiavi di lettura
055600010925     c     K01tbe5       KLIST                                                  *tntbe01l
055700010925     c                   KFLD                    tbecod                          -tabella
055800010925     c                   KFLD                    tbeke1                          -chiave uno
055900010925     c                   KFLD                    tbeke2                          -chiave due
056000010925     c                   KFLD                    tbelin                          -lingua
056100010925     c                   KFLD                    tbesif                          -s.informativo
056101130806     c     kaco          klist
056102130806     C                   kfld                    kkut
056103130806     C                   kfld                    dutkci
056104130806     C                   kfld                    kksc
056200010925      *
056300011108      * Reperisce dati utente
056400020715
056500020715     c     *dtaara       define    §azute        azuteds
056600020715     c     *dtaara       define    §datiute      ddatiute
056700020715
056800020715     c                   in(E)     *dtaara
056900020715     c                   if        %Error or rsut = *blanks
057000020715     c                   clear                   tibs34ds
057100020715     c                   call      'TIBS34R'
057200020715     c                   parm                    Tibs34Ds
057300020715     c                   in        *dtaara
057400020715     c                   endif
057500011106      *
057600011106      * Sistema informativo
057700011106     C                   movel     knsif         Sif              10
057800011106     C
057900000000     C                   ENDSR
058000000000     O*--------------------------------------------------------------------------------------------*
058100010926** -OPZ-
058200120413            Inserimento
058300120413              Modifica
058400120413             Annullato
058500120413Inserimento-COPIATI I VALORI STANDARD
058600000000** -MSG-
058700011105TNTB17R- Immettere il codice tabella
058800011105TNTB17R- Descrizione obbligatoria
058900011108TNTB17R- L'importo minimo non puo' essere maggiore di un importo massimo
059000011108TNTB17R- L'importo massimo 1 non puo' essere maggiore del massimo 2
059100120410TNTB17R- Linea di arrivo errata
059200130806TNTB17R- Codice cliente errato
059300130807TNTB17R- Linea di arrivo obbligatoria se immesso il Codice Cliente
