000100120516      *--------------------------------------------------------------*
000200000725     H DECEDIT('0,') DATEDIT(*DMY.)
000300141106     h dftactgrp(*no) actgrp(*caller)  BNDDIR('TIBS':'UBBNDDIR')
000400120516ab   H BNDDIR('TIBS') EXTBININT(*YES)
000500000809      * FNLV24R *----------------------------------------------------*
000600021129      *      GESTIONE ANAGRAFICA AUTOTRASP/COOPERATIVE
000700911204      *--------------------------------------------------------------*
000800120626      *  26.6.2012 elimnata la gestione delle zone  V3CZR1->5
000900120626      *--------------------------------------------------------------*
001000940809     FFNLV24D   CF   E             WORKSTN
001100940809     F                                     SFILE(LV24S02:NRR2)
001200090401     Faitra03l  IF   E           K DISK    usropn
001300090401     Faitra05l  IF   E           K DISK    usropn
001400090401     f                                     rename(aitra000:aitra500)
001500090113     Fansif01l  IF   E           K DISK    usropn
001600080107     Fazorg01L  IF   E           K DISK
001700090330     Fanrco98j  IF   E           K DISK
001800120216     FTABEL00F  IF   E           K DISK
001900030128     F*NBLP34L  IF   E           K DISK    usropn
002000020419     FfiFGT01L  IF   E           K DISK    usropn
002100070723     Ffiapd22l  UF   E           K DISK    commit(flgcmt) usropn
002200070723     Ffiapd31l  UF   E           K DISK    commit(flgcmt) usropn
002300070723     Ffiapd51l  UF   E           K DISK    commit(flgcmt) usropn
002400130729     Ffiapd41L  UF A E           K DISK    commit(flgcmt) usropn
002500070208     Ffiapd01L  UF A E           K DISK    commit(flgcmt) usropn
002600090610     Ffiadt01l  iF   E           K DISK
002700161003     Ftntbe01l  iF   E           K DISK
002800120217     fqsysprt   o    f  132        printer infds(stamp1) usropn
002900940809     D*
003000120217     d stamp1          ds
003100120217     d  line                 367    368b 0
003200120217      *
003300120521      ***
003400120521     D Ordinamento     S              1
003500120521      *  1=Autista    - niente PDA
003600120521      *  2=RagSociale - niente PDA
003700120521      *  3=Autista    - NO PDA
003800120521      *  4=Autista    -    PDA
003900120521      *  5=RagSociale - NO PDA
004000120521      *  6=RagSociale -    PDA
004100120521      ***
004200160921     D si              S              1
004300160921     D citaff          S              1
004400120521     D Ordina_PDA      S              1
004500120521     D Ordina_COD      S              1
004600120521     D savKK           S                   like(*inKK)
004700120522     D savKI           S                   like(*inKI)
004800120521      *
004900141118     D Leng            s              2s 0
005000141106     D EsitoCel        s              4s 0
005100141106     d V1err           s              1
005200141106     d V1InCellOp      s              1    inz('0')
005300030203     D com03           S              3    inz
005400120220     D tipoelenco      S             13
005500120216     D societa         S              3
005600090113     d cmd             s             50
005700090113     d cmd1            s             50    dim(01) ctdata perrcd(1)
005800090113     d cmd2            s             50    dim(01) ctdata perrcd(1)
005900171102     D MSG             S             78    DIM(40) CTDATA PERRCD(1)
006000950314RM*  D L1              S              3  0 DIM(30) ASCEND
006100950314RM*  D L6              S              3  0 DIM(30) ASCEND
006200940809     D*
006300901219     D                 DS
006400940809     D  KKEY                   1      8
006500901219     D  FIL                    1      3
006600910110     D  ZONA                   4      5
006700060203      *
006800060203     D KPJBA         E DS
006900060203      * - Parametri x Controllo profilo utenti
007000060203     d TIBS34ds      e ds
007100060203      * - Ds di riferimento al file esterno AZUTE00F
007200060203     d AZUTEds       e ds                  extname(AZUTE00F)
007300060203     d dUTE01        e ds
007400060203      * - Ds per dati organigramma
007500060203     d DDatiUte      e ds
007600131008     d tibs42ds      e ds
007700131008     d uteautds      e ds
007800120216     d ds3i          e ds
007900130729     D tibs02ds      E DS
008000130726     d dVpolv24      e ds
008100130726     D  fio                    1     60  0 dim(20)
008200060203      *
008300120306     d dsAIATS       e ds                  extname(AIATS00F)
008301170510     D kpjbusav        S                   like(kpjbu)
008400120628     D cod_societa     S                   like(parsoc)
008500120306      *
008600031121      * Passaggio Parametri al pgm FNLV55R
008700031121     D FNLV55DS      e ds                  inz
008800071203      * Passaggio Parametri al pgm FNLV26R x for. fatturazione
008900071203     D FNLV26DS      e ds                  inz
009000090330      * Passaggio Parametri al pgm FNLV27R x for. contratto e tariffa
009100090330     D FNLV27DS      e ds                  inz
009200060203      *
009201170515     D FNLV37DS      e ds
009300070723     D OG148         E DS                  INZ
009400070108     D OG146         E DS                  INZ
009500070723     D OG150         E DS                  INZ
009600050518     D dapdflr       E DS
009700130729     D dapd4tel      E DS
009800050518     D fnlv24ds      E DS
009900030723     D tibs36ds      E DS
010000940909     D* DS PER TRUL06R - CARICAMENTO £X
010100940909     D DSUL06        E DS                  EXTNAME(TRUL06DS)
010200950314RM*  D  LIN                    1     90  0
010300950314     D                                     DIM(30)
010400060203      *
010500060203      * Tabella "03" = Codici Zona
010600060203     d ds03          e ds                  inz
010700161003     d dats          e ds                  inz
010800161108     d daat          e ds                  inz
010900170508      * Pgm variazioni ds necessarie registrazioni
011000170508     d fiapdds       e ds                  extname(fiapd00f)
011100170508     d fiapd4ds      e ds                  extname(fiapd40f)
011200170508     d fnlv36ds      e ds                  inz
011300170508     d fiapda        e ds                  extname(fiapd00f)
011400170508     d                                     prefix(a)
011500170508     d fiapdb        e ds                  extname(fiapd00f)
011600170508     d                                     prefix(b)
011700170508     d fiapd4A       e ds                  extname(fiapd40f)
011800170508     d                                     prefix(a)
011900170508     d fiapd4b       e ds                  extname(fiapd40f)
012000170508     d                                     prefix(b)
012100060203      *
012200950314RM*  D                 DS
012300950314RM*  D  V1CPDR                 1      7
012400950314RM*  D  V1FPAD                 1      3  0
012500950314RM*  D  V1CPAD                 4      7
012600120216
012700120216     D partraz         DS
012800120605     d  parsoc                        3  0
012900120502     D  pariva                       16
013000120502     D  pardtin                       8s 0
013100120502     D  pardtfi                       8s 0
013200120502     d  esito                         1
013300120502     d  errmsg                       75
013301171212      *
013302171212     D param           DS
013303171212     D  Icodaut                       7s 0
013304171212     D  Ocodaut                       7s 0
013305171212     D  Onumcel                      16
013306171212     d  Oesito                        1
013400120516ab   ***************************************************************************
013500120516ab   **
013600120516ab   ** Prototipi.
013700120516ab   **
013800120516ab   ***************************************************************************
013900120516ab    **
014000120516ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
014100120516ab    /DEFINE DFTACTGRP_NO
014200120516ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
014300120516ab    /UNDEFINE DFTACTGRP_NO
014400141106       // - Parametri x UBCHKCEL
014500141106      /copy gaitrasrc/srcProtoPi,UBCHKCEL
014600141106       // - Prototipo srvpgm
014700141106      /copy gaitrasrc/srcProtoPr,UBCHKCEL
014800141106
014900120516ab   **
015000120516ab   ***************************************************************************
015100120516ab   **
015200120516ab   ** Strutture dati.
015300120516ab   **
015400120516ab   ***************************************************************************
015500120516ab   **
015600120516ab   ***************************************************************************
015700120516ab   **
015800120516ab   ** Campi.
015900120516ab   **
016000120516ab   ***************************************************************************
016100120516ab   D TibsSof_esito...
016200120516ab   D                 S             10I 0 IMPORT
016300120516ab   D idSocieta...
016400120518EDPDCD                 S              3A
016500120516ab   **
016600120518EDPDCd dtVALIDITA      s               D
016700120518EDPDCd VALdatFINE      s               D
016800120518EDPDCd VALdatINIZ      s               D
016900120518ab   **
017000120522     d Ha_CONTRATTO    C                   'esiste contratto  TRAZIONISTA'
017100031121      *
017200031121      * DS per inversione data
017300031121     D WLBDAT          DS                  INZ
017400031121     D  G02DAT                 1      8  0
017500031121     D  G02INV                 9     16  0
017600031121     D  G02ERR                17     17
017700031121     D  G02TGI                18     22  0
017800031121      *
017900040421     D mess_err        S              1    INZ(*ON)
018000060203     D wDATE           S              8  0 inz
018100060203     D Data_Lim        S              8  0 INZ
018200060203     d dataiso         s               d   datfmt(*iso)
018300071205     d NotUsed         S             16A
018400071205     d ReturnSize      S              9B 0
018500071205     d SizeList        S              9B 0
018600071205     d WrkSort         S              1
018700071205     d*---
018800140219     d nrr2            S              4  0
018900140219     d stmrec1         S              4  0
019000071205     d*---
019100120326     d sav3cksc        S                   like(v3cksc)
019200120326     d sav3ccsf        S                   like(v3ccsf)
019300120326     d*---
019400071205     d MaxKey          C                   2
019500071205     d Ascendente      C                   1
019600071205     d Discendente     C                   2
019700071205     d Carattere       C                   6
019800071205     d Put             C                   1
019900071205     d EndPut          C                   2
020000071205     d Get             C                   3
020100071205     d Numerico        C                   8
020200071205     d*---
020300071205     d SflRcd          DS
020400120517     d  h1ord
020500071205     d  vidsel
020600071205     d  vscpdr
020700071205     d  vscrsc
020800120522     d  vscPDAsoc
020900071205     d  vscksc
021000071205     d  vscpkl
021100071205     d  vscvlu
021200120522     d  vscptp
021300120522     d  vsctpm
021400160426     d  vsctur
021500120224     d  VTRAZ
021600120224     d  VSOCOPE
021700120224     d  VSCKSCD
021800071205     d  Selected                      1A
021900071205
022000071205      /COPY QSYSINC/QRPGLESRC,QLGSORT
022100071205     d QLGKL                         16    DIM(MaxKey)
022200071205     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
022300071205     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
022400071205     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
022500071205     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
022600071205      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
022700071205      /COPY QSYSINC/QRPGLESRC,QUSEC
022800920921     C****************************************************************
022900920921     C*  RIEPILOGO INDICATORI
023000920921     C****************************************************************
023100920921     C* 11    - IMMISSIONE
023200920921     C* 12    - VARIAZIONE
023300920921     C* 13    - ON  INDICA CHE IL PROGRAMMA E' RICHIAMATO
023400920921     C* 15    - ON  INDICA CHE LA FILIALE PASSATA E' 0
023500920923     C* 17    - OFF INDICA CHE NON E' STATA EFFETTUATA NESSUNA
023600920923     C*         SELEZIONE ALL'INTERNO DEL SUBFILE
023700021129     C* 21    - INDICA SE IL COD. AUTO/COOP  INSERITO ESISTE IN TABEL
023800920921     C* 30/33 - DI COMODO
023900000808     C* 40    - ERRORE
024000000726     C* 41    - RIATTIVAZIONE
024100000726     C* 42    - SOTTOMETTO PROGRAMMA DI TRASMISSIONE
024200920921     C* 45/50 - DI COMODO
024300920922     C* 53/58 - ERRORE
024400030129     C* 90    - ERRORE GENERICO
024500010206     C* 91/95 - POSIZIONE CURSORE NELLE ZONE
024600021129     C* 98    - ERRORE DURANTE LA SCRITTURA DI fiapd00F
024700920921     C*****************************************************************
024800000000     C     *ENTRY        PLIST
024900000000     C                   PARM                    KPJBA
025000060203      *
025100060203      * Reperisco dati job
025200060203     c                   exsr      DatiJob
025300060203      *
025400060203     C                   MOVEL     RSUT          V1CDSA
025500021129     C                   MOVEL     KPJBU         fnlv24ds
025501170516     C                   MOVE      KPJBU         vediturno         1
025600120723      *
025700070208     c                   if        d24cmt = '1'
025800070208     c                   eval      flgcmt = *on
025900070208     c                   else
026000070208     c                   eval      flgcmt = *off
026100070208     c                   end
026200120723      *
026300070208     c                   open      fiapd01l
026400070723     c                   open      fiapd22l
026500070723     c                   open      fiapd31l
026600070723     c                   open      fiapd51l
026700130729     c                   open      fiapd41l
026800120723      *
026900020419     C* P R O G R A M M A   N O N   R I C H I A M A T O
027000021129     c                   if        d24flg = *blanks
027100120723      *
027200030721     c* ultima posizione kpjbu mi dice se COOP o AUTOTRASP. o aff/def
027300030127     c                   move      kpjbu         v1ctip
027400030210     c                   if        v1ctip = *blanks
027500030210     c                   eval      v1ctip = 'A'
027600030210     c                   end
027700120723      *
027800120723      ***    = Autista di Città
027900090401     c                   if        v1ctip = 'A'
028000090401     c                   open      aitra03l
028100090401     c                   end
028200120723      *
028300120723      ***    = Affl.Defl.
028400090401     c                   if        v1ctip = 'D'
028500120216      * reperisco codice società traini
028600120216     c                   move      '3I'          tblcod
028700120216     c                   movel(p)  '1'           tblkey
028800120216     c     ktab          chain     tabel00f
028900120216     c                   if        %found(tabel00f)
029000120216     c                   movel     tbluni        ds3i
029100120216     c                   eval      societa = %subst(§3IBST: 7: 3)
029200120216     c                   endif
029300090401     c                   open      aitra05l
029400090401     c                   end
029500091106     c*
029600090504     c                   if        simfel = 0
029700090504     c                   z-add     utefil        simfel
029800090504     c                   endif
029801170519     c*forzatura provvisoria accende indicatore per gestione richiamo pgm
029802170519      * visualizzazione variazioni
029803170519     c                   if        %subst(knmus:1:3) = 'EDP'
029804170519     c                   seton                                        26
029805170519     c                   endif
029900120723     c*
030000090113     c* se  sistema informativo di filiale e gestione COOP o AFF/DEF
030100090113     c* effettua ovrdbf x ANSIF00F per gestire le società
030200090113     c                   if        v1ctip = 'C' or
030300090113     c                             v1ctip = 'D'
030400120723     c*
030500090113     c                   if        %subst(knsif:7:1) = 'P'
030600090113     C                   movea     cmd2(1)       cmd
030700090113     c                   else
030800090113     C                   movea     cmd1(1)       cmd
030900090113     c                   endif
031000120723     c*
031100090113     C                   movel(p)  cmd           comman
031200090113     C                   call      'QCMDEXC'
031300090113     C                   parm                    comman           80
031400090113     C                   parm      48            lung             15 5
031500090113     C*
031600090113     c                   endif
031700090113     c                   open      ansif01l
031800120723     c*
031900080911     c* se coop non visualizzo massa complessiva automezzo
032000080911     c     v1ctip        comp      'C'                                    19
032100030128     c*
032200020419     c                   open      fifgt01l
032300940909     C* CARICO LA TABELLA '£1'
032400940909     C                   MOVE      '£1'          D06COD
032500940909     C                   MOVEL     SIMFEL        D06KEY
032600940909     C                   MOVEL     DSUL06        KPJBU
032700940909     C                   CALL      'TRUL06R'
032800940909     C                   PARM                    KPJBA
032900940909     C                   MOVEL     KPJBU         DSUL06
033000940909     C                   MOVEA     LIN           L1
033100120723     c*
033200950314RM*  C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
033300060203     c                   if            DUTlpo = '2'
033400060203     c                             or  DUTlpo = *blanks
033500950314RM*  C                   MOVE      '£6'          D06COD
033600060203RM*  c                   movel     DUTpou        D06KEY
033700950314RM*  C                   MOVEL     DSUL06        KPJBU
033800950314RM*  C                   CALL      'TRUL06R'
033900950314RM*  C                   PARM                    KPJBA
034000950314RM*  C                   MOVEL     KPJBU         DSUL06
034100950314RM*  C                   MOVEA     LIN           L6
034200950316RM*  C*
034300060203RM*  c                   move      DUTpou        CDU
034400120723     c*
034500950316RM*  C                   ELSE
034600950316RM*  C                   MOVE      SIMFEL        CDU
034700950314RM*  C                   END
034800120723     c*
034900030723      *se richiamato per affluenze defluenze imposta il terminal di partenza
035000030723     c                   if        v1ctip = 'D'
035100030723     C                   CLEAR                   Tibs36Ds
035200030723     C                   EVAL      I36ute = knmus
035300030723     C                   CALL      'TIBS36R'
035400030723     C                   PARM                    tibs36ds
035500030723     c                   move      o36tfp        cdu
035600030723     c                   endif
035700120723     c*
035800020419     c                   else
035900931130     C* 13 ON - P R O G R A M M A   R I C H I A M A T O
036000120224     c                   movel     d24fil        v1fpad
036100120224     c                   movel     d24tip        v1ctip
036200950314RM*  C                   MOVEL     '?'           V1CPAD
036300920921     C                   SETON                                        13
036400120723     c*
036500920917     C* CONTROLLO SE LINEA PASSATA E' 0
036600021129     C     d24fil        COMP      0                                  15
036700920922     C                   END
036800920917     C*
036900130731      * Aggancio tabella "VPOLV24" per reperimento filiali che utilizzano
037000130731     C                   CLEAR                   tibs02ds
037100130731     C                   MOVEL     'C'           T02MOD
037200130731     C                   MOVEL     KNSIF         T02SIF
037300130731     C                   MOVEL     'VPO'         T02COD
037400161109     C                   MOVEL(p)  'FNLV24R'     T02KE1
037500130731     C                   CALL      'TIBS02R'
037600130731     C                   PARM                    KPJBA
037700130731     C                   PARM                    tibs02ds
037800130731     C     T02ERR        IFEQ      *BLANKS
037900130731     C                   MOVEL     T02UNI        DVPOlv24
038000130731     C                   ELSE
038100130731     C                   CLEAR                   DVPOlv24
038200130731     C                   ENDIF
038300920918     C     INIZIO        TAG
038400021129RM*  C  N13              MOVEL     *BLANKS       V1CPAD
038500950314RM*  C  N13              MOVEL     '?'           V1CPAD
038600950316RM*  C  N13              MOVE      CDU           V1FPAD
038700021129     C*
038800030723     c                   select
038900030723     C                   when      V1CTIP = 'A'
039000021129RM*  C                   eval      v1dtip = 'AUTOTRASPORTATORE'
039100120220     c                   eval      tipoelenco = 'Autisti Città'
039200120723     c                   seton                                        23
039300030723     C                   when      V1CTIP = 'C'
039400021129RM*  C                   eval      v1dtip = 'COOPERATIVA'
039500120220     c                   eval      tipoelenco = 'Cooperative  '
039600030723     C                   when      V1CTIP = 'D'
039700030723RM*  C                   eval      v1dtip = 'Affluenze/Defluenze'
039800120220     c                   eval      tipoelenco = 'Autisti Af/De'
039900120723     c                   seton                                        22
040000021129     C                   END
040100920918     C*
040200940809     C                   MOVEL     'S'           WRIEM
040300920918     C     FOR01         TAG
040400940809     C  N13WRIEM         IFEQ      'S'
040500940809     C                   WRITE     LV24T01
040600940809     C                   MOVEL     *BLANKS       WRIEM
040700940809     C                   ENDIF
040800940809     C  N13              EXFMT     LV24D01
040900940809     C*
041000950314RM*  C                   SETOFF                                       285758
041100940809     C*
041200071205     C* CMD3 - FINE LAVORO
041300071205     C   KC              GOTO      FINE
041400920918     C*
041500021129RM*  C* CONTROLLO FILIALE AUTO/COOP  SE PROGRAMMA NON RICHIAMATO
041600950314RM* 1C     *IN13         IFEQ      *OFF
041700021129RM*  C* La filiale AUTO/COOP  può essere = 0 solo se richiesta la ricer
041800950314RM*  C* ca con ?
041900950314RM* 2C     V1FPAD        IFEQ      *ZEROS
042000950314RM*  C     '?'           SCAN      V1CPAD                                 31
042100140219RM* 3C     *IN31         IFEQ      *On
042200950314RM*  C                   MOVEL     MSG(14)       V1CMSG
042300950314RM*  C                   SETON                                        2857
042400001122     C                   GOTO      FOR01
042500950314RM* 3C                   END
042600021016      *
042700021016      *
042800950314RM*  C                   ELSE
042900031121     C*
043000031121     C*  SOLO PER AFFLUENZE/DEFLUENZE
043100031121     C*  il codice fornitore deve appartenere ad un terminal
043200031121     C                   IF        v1ctip='D'
043300031121     c                   movel     v1fPad        cterm             3 0
043400031121     c                   movel     cterm         svterm            3 0
043500031121     C                   reset                   FNLV55DS
043600031121     C                   eval      D55lin  = cterm
043700031121     C                   eval      D55drf  = wDATE
043800031121     C                   call      'FNLV55R'
043900031121     C                   parm                    FNLV55DS
044000031121     C                   IF        d55tfp <> cterm  and
044100031121     C                             d55tfa <> cterm
044200151124     C                   reset                   FNLV55DS
044300151124     C                   eval      D55lin  = cterm
044400151124     c                   move      wdate         dataiso
044500151124     c     dataiso       adddur    30:*d         dataiso
044600151124     c                   move      dataiso       d55drf
044700151124     C                   call      'FNLV55R'
044800151124     C                   parm                    FNLV55DS
044900151124     C                   IF        d55tfp <> cterm  and
045000151124     C                             d55tfa <> cterm
045100031121     C                   MOVEL     MSG(12)       V1CMSG
045200031121     C                   SETON                                        2857
045300031121     C                   GOTO      FOR01
045400151124     c                   end
045500031121     c                   end
045600031121     c                   end
045700031121     C*
045800021016      *
045900021129RM*  C* Filiale AUTO/COOP  diversa da 0:
046000030129     c                   select
046100060203     c                   when      DUTlpo = '2' or DUTlpo = *blanks
046200950314RM*  C* deve essere in £6 se secondo livello
046300950314RM*  C     V1FPAD        LOOKUP    L6                                     45
046400060203RM*  C                   when      DUTlpo = '1'
046500950314RM*  C* deve essere in £1 se primo livello
046600950314RM*  C     V1FPAD        LOOKUP    L1                                     45
046700060203RM*  C                   when      DUTlpo = 'S'
046800030129RM*  C* deve essere in azorg01l
046900030129RM*  C     V1FPAD        chain     azorg01l
047000030129     c                   if        %found(azorg01l)
047100030203     c                   if        orgfag <> 'A' and orgfag <> 'F'
047200030203     c                   setoff                                       45
047300030203     c                   else
047400030129     c                   seton                                        45
047500030203     c                   end
047600030129     c                   else
047700030129     c                   setoff                                       45
047800030129     c                   end
047900030129RM* 3C                   ENDsl
048000021016      *
048100950314RM*  C* Filiale non presente in tabella
048200950314RM* 3C     *IN45         IFEQ      *OFF
048300950314RM*  C                   MOVEL     MSG(12)       V1CMSG
048400950314RM*  C                   SETON                                        2857
048500950314RM*  C                   GOTO      FOR01
048600950314RM* 3C                   END
048700950314RM* 2C                   END
048800950314RM* 1C                   END
048900021129     C* CODICE AUTO/COOP  SEMPRE > 0
049000950314RM*  C     V1CPAD        IFEQ      *BLANKS
049100950314RM*  C     V1CPAD        OREQ      *ZERO
049200940809     C                   MOVEL     MSG(1)        V1CMSG
049300950314     C                   SETON                                        28  58
049400920918     C                   GOTO      FOR01
049500920918     C                   END
049600050524     c* verifico se filiale abilitata al parcel + messaggerie
049700050524     c                   exsr      srtpm
049800130729     c* verifico se filiale abilitata all'inserimento del numero di telefono VPO
049900130729     c                   if        v1fpad > 0
050000130729RM*  C     V1FPAD        LOOKUP    fio                                    52
050100130729RM*  C  n52999           LOOKUP    fio                                    52
050200130729     c                   endif
050300931130     C**
050400931130     C** R I C E R C A   ' ? '
050500931130     C**
050600950314RM*  C     '?'           SCAN      V1CPAD                                 31
050700911204     C   31              DO
050800920918     C* CARICO SUBFILE
050900911204     C                   EXSR      CARICA
051000920918     C* SE NON CARICA NIENTE ESCO
051100940809     C     NRR2          IFEQ      0
051200920918     C                   GOTO      FINE
051300920918     C                   END
051400920918     C*
051500940809     C                   WRITE     LV24T01
051600071205     C                   Z-ADD     nrr2          STMREC1
051700920923     C     FOR03         TAG
051800120521      *
051900120521      *  1=Autista    - niente PDA
052000120521      *    2=RagSociale - niente PDA
052100120521      *  3=Autista    - NO PDA
052200120521      *  4=Autista    -    PDA
052300120521      *    5=RagSociale - NO PDA
052400120521      *    6=RagSociale -    PDA
052500120521      **
052600120521     c                   if        Ordinamento = '1'
052700120522     c                   setoFF                                       51  81
052800120522     c                   setoN                                            35
052900120521      **
053000120521     c                   elseif    Ordinamento = '2'
053100120522     c                   setoN                                        51  36
053200120522     c                   setoFF                                           81
053300120521     c**
053400120521     c                   elseif    Ordinamento = '3'
053500120521     c                   setoff                                       51
053600120522     c                   setoN                                        81  37
053700120521     c**
053800120521     c                   elseif    Ordinamento = '4'
053900120522     c                   setoff                                       51  81
054000120522     c                   seton                                            37
054100120521     c**
054200120521     c                   elseif    Ordinamento = '5'
054300120522     c                   seton                                        518137
054400120521     c**
054500120521     c                   elseif    Ordinamento = '6'
054600120522     c                   setoff                                       81
054700120522     c                   seton                                        51  37
054800120521     c**
054900120521     c                   end
055000120521     C                   WRITE     LV24Z02
055100120521      *
055200940809     C                   EXFMT     LV24C02
055300120521      *
055400920918     C* CMD3 - FINE LAVORO
055500920918     C   KC              GOTO      FINE
055600920918     C* CMD12 - RITORNO
055700920921     C   KL
055800920921     CANN13              GOTO      INIZIO
055900941006     C   KL
056000941006     CAN 13              DO
056100021129     C                   CLEAR                   fnlv24ds
056200941006     C                   MOVEL     *BLANKS       KPJBU
056300021129     C                   MOVEL     fnlv24ds      KPJBU
056400941006     C                   GOTO      FINE
056500941006     C                   ENDDO
056600120217     C* F07 /f08 stampa elenco o stampa elenco trazionisti
056700120217if  1C                   IF        *inkg or *inkh
056800120217     c                   open      qsysprt
056900120217     c                   z-add     90            line
057000120217     C                   EXSR      stampa
057100120217     c                   close     qsysprt
057200120220     C                   GOTO      For03
057300120217e   1C                   ENDIF
057400071205     C* F11 = Cambia ordinamento
057500120521     C* F15 = Cambia ordinamento x PDA oppure NO
057600120522if  1C                   IF        *INKK or *INKI
057700120521      * x ordinamenti intestazioni campi di ordinamento (reset)
057800120521     c                   setoff                                       37
057900120521if  1C                   IF        *INKK
058000120521     c                   setoff                                       3536
058100120521     c                   end
058200120521     C                   EXSR      Ordina
058300071205     C                   GOTO      For03
058400071205e   1C                   ENDIF
058500120521     C*
058600920918     C* CONTROLLO DATI SUBFILE
058700920923     C                   SETOFF                                       3017
058800940809     C                   READC     LV24S02                                30
058900920918     C     *IN30         DOWEQ     '0'
059000920918     C*
059100920918     C     VIDSEL        IFEQ      '1'
059200920923     C                   SETON                                        17
059300021129     C* SE PROGRAMMA RICHIAMATO PASSO PRIMO CODICE AUTO/COOP
059400920918     C*    SELEZIONATO E ESCO
059500920921     C     *IN13         IFEQ      '1'
059600021129     C                   MOVEL     VSCPDR        d24pdr
059700021129     C                   MOVEL     VSCRSC        d24rsc
059800021129     C                   MOVEL     fnlv24ds      KPJBU
059900940929     C                   GOTO      FINE
060000920922     C*
060100920918     C                   ELSE
060200940809     C                   MOVEL     VSCPDR        V1CPDR
060300030127     c* autotrasportatore
060400030721     c                   if        v1ctip = 'A'
060500920923     C* SR 3° VIDEATA
060600920918     C                   EXSR      VIDEO3
060700030127     c* cooperativa
060800030127     c                   else
060900030127     C* SR 4° VIDEATA
061000030127     C                   EXSR      VIDEO4
061100030127     c                   end
061200920921     C* CMD3 - FINE LAVORO
061300920921     C   KC              GOTO      FINE
061400920918     C                   END
061500920918     C                   END
061600920918     C*
061700940809     C                   READC     LV24S02                                30
061800920918     C                   END
061900920923     C* 17 - OFF SE NON E' STATA EFFETTUATA NESSUNA SELEZIONE
062000920923     C*      RIEMETTO IL SUBFILE
062100950314RM*  C  N17              GOTO      FOR03
062200920923     C*
062300920923     C  N13              GOTO      INIZIO
062400911204     C                   END
062500911204      *-----------------------------------------------------*
062600000726      * Si testa se il campo contiene solo numeri e si      *
062700021129      * ricerca su fiapd00F codice AUTO/COOP                *
062800911204      *-----------------------------------------------------*
062900920921     C  N13
063000920921     CANN31              DO
063100920922     C*
063200021129     C* CONTROLLO VALIDITA' CODICE AUTO/COOP
063300940809     C                   TESTN                   V1CPDR               50
063400950314RM*  C                   MOVE      V1CPDR        W001A             1
063500950314RM*  C     *IN50         IFEQ      *OFF
063600950314RM*  C     W001A         ORLT      '0'
063700940809     C                   MOVEL     MSG(2)        V1CMSG
063800950314     C                   SETON                                        58  28
063900950314     C                   GOTO      FOR01
064000950314RM*  C                   END
064100030127     c* autotrasportatore
064200030721     c                   if        v1ctip = 'A'
064300920923     C* SR 3° VIDEATA
064400920921     C                   EXSR      VIDEO3
064500030127     c* cooperativa
064600030127     c                   else
064700030127     C* SR 4° VIDEATA
064800030127     C                   EXSR      VIDEO4
064900030127     c                   end
065000920922     C  NKC              GOTO      INIZIO
065100911205     C                   END
065200920921     C*
065300911205     C     FINE          TAG
065400021129     C   13              MOVEL     fnlv24ds      KPJBU
065500000726      *
065600901108     C                   SETON                                        LR
065700060203      *
065800071205      *-------------------------------------------------------------------------
065900120521      * TASTI FUNZIONALI di ordinamento
066000071205      *-------------------------------------------------------------------------
066100120521     C     Combinazioni  BEGSR
066200120517      *
066300120521      *  Che tipo di ordinamento deve avere ?
066400120521      *  1=Autista    - niente PDA
066500120521      *    2=RagSociale - niente PDA
066600120521      *  3=Autista    - NO PDA
066700120521      *  4=Autista    -    PDA
066800120521      *    5=RagSociale - NO PDA
066900120521      *    6=RagSociale -    PDA
067000120521      **
067100120521      **    F11
067200120521     c                   if         *INKK
067300120521     c                   if        ordinamento = '1' or
067400120521     c                             ordinamento = '3' or
067500120521     c                             ordinamento = '4'
067600120521     c                   eval      ordinamento = '2'
067700120521      **
067800120521     c                   elseIF    ordinamento = '2' or
067900120521     c                             ordinamento = '5' or
068000120521     c                             ordinamento = '6'
068100120521     c                   eval      ordinamento = '1'
068200120521     c                   end
068300120521     c                   endif
068400120521      **
068500120521      **   F15
068600120522     c                   if         *INKI
068700120521      **
068800120521     c                   IF        ordinamento = '1' and ordina_PDA = 'N'
068900120521     c                              or
069000120521     c                             ordinamento = '4'
069100120521     c                   eval      ordinamento = '3'
069200120521      **
069300120521     c                   elseIF    ordinamento = '1' and ordina_PDA = 'S'
069400120521     c                              or
069500120521     c                             ordinamento = '3'
069600120521     c                   eval      ordinamento = '4'
069700120521      **
069800120521     c                   elseIF    ordinamento = '2' and ordina_PDA = 'N'
069900120521     c                              or
070000120521     c                             ordinamento = '6'
070100120521     c                   eval      ordinamento = '5'
070200120521      **
070300120521     c                   elseIF    ordinamento = '2' and ordina_PDA = 'S'
070400120521     c                              or
070500120521     c                             ordinamento = '5'
070600120521     c                   eval      ordinamento = '6'
070700120521      **
070800120521     c                   end
070900120521      **
071000120521     c                   endIF
071100120521      *
071200120521     C                   ENDSR
071300120521      *-------------------------------------------------------------------------
071400120521      * Ordina - TASTO FUNZIONE F11 --> Ragione sociale/cod. aut.
071500120521      *-------------------------------------------------------------------------
071600120521     C     Ordina        BEGSR
071700120521      *
071800120521     c                   exsr      COMBINAZIONI
071900120521      **
072000071205      * IMPOSTO ENTRATA AD 'A'
072100071205     C                   EVAL      rec2  = 1
072200071205      *
072300071205     C                   CLEAR                   QLGSCB
072400071205     C                   CLEAR                   QLGSCB00
072500120521      *
072600120521      *  per attivare la prima chiave
072700120521     c                   Exsr      AGG_PrimaKEY
072800120521      *
072900120517     ** 2 campi chiave. il primo è sempre un ordinamento occulto per
073000120517     **    adesso è solo per gli AUT con PDA (prima si vuole far vedere
073100120517     **      sempre quelli che non ce l'hanno) --> che hanno "N".
073200120517     C                   EVAL      QLGNBRK    = 2
073300120517      *
073400120517     ** prima x PDA è a posizione 1, numerico, 1 byte, descending.
073500120517     C                   EVAL      QLGSP      = 1
073600120517     C                   EVAL      QLGSS      = %SIZE(H1ORD)
073700120517     C                   EVAL      QLGDT      = Numerico
073800120521     C                   EVAL      QLGSO      = Ascendente
073900120521      **
074000120521     c                   if        ordinamento = '3'  or
074100120521     c                             ordinamento = '5'
074200120521     c                   eval      ordina_PDA = 'N'
074300120521     C                   EVAL      QLGSO      = Discendente
074400120521     c                   elseIF    ordinamento = '4'  or
074500120521     c                             ordinamento = '6'
074600120521     c                   eval      ordina_PDA = 'S'
074700120521     C                   EVAL      QLGSO      = Ascendente
074800120521     c                   end
074900120521      **
075000120517     C                   EVAL      QLGKL(1)   = QLGSKL
075100071205
075200120521     c                   if        ordinamento = '1'  or
075300120521     c                             ordinamento = '3'  or
075400120521     c                             ordinamento = '4'
075500071205     ** IL P.O è a posizione 3, numerico, 3 byte, ascending.
075600120517     C                   EVAL      QLGSP      = 3
075700071205     C                   EVAL      QLGSS      = %SIZE(vscpdr)
075800071205     C                   EVAL      QLGDT      = Numerico
075900071205     C                   EVAL      QLGSO      = Ascendente
076000120517      *
076100120521     c                   elseIF    ordinamento = '2'  or
076200120521     c                             ordinamento = '5'  or
076300120521     c                             ordinamento = '6'
076400071205     ** La ragione sociale è a posizione 6, 20 byte, char, ascending.
076500120517     C                   EVAL      QLGSP      = 10
076600071205     C                   EVAL      QLGSS      = %SIZE(vscrsc)
076700071205     C                   EVAL      QLGDT      = Carattere
076800071205     C                   EVAL      QLGSO      = Ascendente
076900071205     C                   ENDIF
077000120521
077100120521     C                   EVAL      QLGKL(2)   = QLGSKL
077200071205
077300071205      * Load other sort parameters.
077400071205     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
077500071205     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
077600071205     C                   EVAL      QLGRT     = 8
077700071205     C                   EVAL      QLGOKL    = 80
077800071205     C                   EVAL      QLGLKE    = 16
077900071205     C                   EVAL      QLGLSS    = 290
078000071205
078100071205      * Initialize Sort I/O API fields.
078200071205     C                   EVAL      QLGRL00  = QLGRL
078300071205     C                   EVAL      QLGRC00  = 1
078400071205     C                   CLEAR                   QUSEI
078500071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
078600071205
078700071205      * First step - Initialize the sort routine.
078800071205     C                   CALL      'QLGSORT'
078900071205     C                   PARM                    QLGSCB
079000071205     C                   PARM                    NotUsed
079100071205     C                   PARM                    NotUsed
079200071205     C                   PARM                    SizeList
079300071205     C                   PARM                    ReturnSize
079400071205     C                   PARM                    QUSEC
079500071205
079600071205      * Next step - Write records to I/O routine.
079700071205     C                   EVAL      QLGRT00 = Put
079800071205
079900140219     C                   DO        STMREC1       x                 4 0
080000071205
080100071205     C     x             CHAIN     LV24S02
080200071205
080300071205     ** Solo le righe con Selected = 'Y' sono riordinate,
080400071205     ** quindi per fare un ordinamento di tutte le righe
080500071205     ** metto 'Y' sempre.
080600071205     C                   EVAL      Selected = 'Y'
080700071205
080800071205     C                   CLEAR                   QUSEI
080900071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
081000071205     C                   CALL      'QLGSRTIO'
081100071205     C                   PARM                    QLGSCB00
081200071205     C                   PARM                    SflRcd
081300071205     C                   PARM                    NotUsed
081400071205     C                   PARM                    SizeList
081500071205     C                   PARM                    NotUsed
081600071205     C                   PARM                    QUSEC
081700071205
081800071205     C                   ENDDO
081900071205
082000071205      * Next step - Signal end of input, clear subfile for reload.
082100071205     C                   EVAL      QLGRT00 = EndPut
082200071205     C                   CLEAR                   QUSEI
082300071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
082400071205
082500071205     C                   CALL      'QLGSRTIO'
082600071205     C                   PARM                    QLGSCB00
082700071205     C                   PARM                    SflRcd
082800071205     C                   PARM                    NotUsed
082900071205     C                   PARM                    SizeList
083000071205     C                   PARM                    NotUsed
083100071205     C                   PARM                    QUSEC
083200071205
083300071205     C                   SETOFF                                       21
083400071205     C                   WRITE     LV24C02
083500071205     C                   SETON                                        21
083600071205     C                   WRITE     LV24T01
083700071205     C                   WRITE     LV24Z02
083800071205
083900071205      * Final step - Write the records back to the subfile.
084000071205     C                   EVAL      QLGRT00 = Get
084100071205
084200071205     C                   DO        STMREC1       x
084300071205     C                   CLEAR                   QUSEI
084400071205     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
084500071205     C                   CALL      'QLGSRTIO'
084600071205     C                   PARM                    QLGSCB00
084700071205     C                   PARM                    NotUsed
084800071205     C                   PARM                    SflRcd
084900071205     C                   PARM                    QLGRL00
085000071205     C                   PARM                    NotUsed
085100071205     C                   PARM                    QUSEC
085200071205      *
085300071205     c                   z-add     x             nrr2
085400120224     c     vtraz         comp      'T'                                    25
085500071205     C                   WRITE     LV24S02
085600071205     C                   ENDDO
085700071205      *
085800071205     C                   ENDSR
085900060203      *---------------------------------------------------------------*
086000120521      * Aggiorna la prima chiave.
086100060203      *---------------------------------------------------------------*
086200120521     c     AGG_PrimaKEY  BEGSR
086300120521      **
086400140219     C                   DO        STMREC1       x
086500120521     C     x             CHAIN     LV24S02
086600120521
086700120521     c                   z-add     0             h1ord
086800120521     c                   eval      ordina_PDA  = 'N'
086900120521      *
087000120521      *
087100120521     c                   if        Ordinamento <> '1' and
087200120521     c                             Ordinamento <> '2'
087300120521      *  ossia se premuto F15
087400120522     c                   if        %subst(vscPDAsoc:1:1) = 'N'
087500120522     c                   z-add     4             h1ord
087600120522     c                   ELSEif    %subst(vscPDAsoc:1:1) = 'O'
087700120522     c                   z-add     3             h1ord
087800120522     c                   ELSEif    %subst(vscPDAsoc:1:1) = 'C'
087900120522     c                   z-add     2             h1ord
088000120522     c                   ELSEif    %subst(vscPDAsoc:1:1) = 'E'
088100120522     c                   z-add     1             h1ord
088200120521     c                   end
088300120521      ***
088400120521     c                   else
088500120521      ***
088600120521     c                   if        Ordinamento  = '3' or
088700120521     c                             Ordinamento  = '5'
088800120521     c                   eval      ordina_PDA  = 'N'
088900120521     c                   end
089000120521      ***
089100120521     c                   if        Ordinamento  = '4' or
089200120521     c                             Ordinamento  = '6'
089300120521     c                   eval      ordina_PDA  = 'S'
089400120521     c                   end
089500120521      ***
089600120521     c                   endIf
089700120521
089800120521     ** Aggiorna e imposta la prima chiave oppure la elimina
089900120521     C                   update    LV24S02
090000120521     C                   ENDDO
090100120521      *
090200120521      *
090300120521     C                   ENDSR
090400120521      *---------------------------------------------------------------*
090500120521      * Reperimento Dati del job (Utente/Operativi)                   *
090600120521      *---------------------------------------------------------------*
090700120521     c     DatiJob       BEGSR
090800120521      *
090900060203     c     *dtaara       define    §azute        azuteds
091000060203     c     *dtaara       define    §datiute      ddatiute
091100060203      *
091200060203     c                   in(E)     *dtaara
091300060203     c                   IF        %ERROR or RSUT = *blanks
091400060203     c                   clear                   Tibs34Ds
091500060203     c                   call      'TIBS34R'
091600060203     c                   parm                    Tibs34Ds
091700060203     c                   in        *dtaara
091800060203     c                   ENDIF
091900060203      *
092000060203     c                   ENDSR
092100940809     C**************************************************************************
092200940809     C*    PULIZIA CAMPI 3°VIDEATA
092300940809     C**************************************************************************
092400920922     C     AZZ           BEGSR
092500940809     C*
092600050225     C                   move      *ZEROS        V3Cgge
092700050523     C                   MOVE      'P'           V3Ctpm
092800160426     C                   MOVE      '1'           V3Ctur
092900050804     C                   move      *ZEROS        V3Ckm
093000050519     C                   MOVE      *BLANKS       V3CRSC
093100021129     C                   MOVE      *BLANKS       V1CRSF
093200940809     C                   MOVEL     *BLANKS       V3CPDD
093300940809     C                   Z-ADD     *ZEROS        V3CKSC
093400120326     C                   Z-ADD     *ZEROS        saV3CKSC
093500940809     C                   MOVE      *BLANKS       V3DKSC
093600940809     C                   MOVE      *BLANKS       V3CCSF
093700120326     C                   MOVE      *BLANKS       saV3CCSF
093800940809     C                   MOVEL     *BLANKS       V3CFGR
093900940809     C                   CLEAR                   V3CCSF
094000940809     C                   Z-ADD     *ZEROS        V3CPKL
094100940809     C                   Z-ADD     *ZEROS        V3CVLU
094200030128     C                   Z-ADD     *ZEROS        V3Chpa
094300030128     C                   Z-ADD     *ZEROS        V3Char
094400941103     C                   MOVEL     'N'           V3CETM
094500941103     C                   MOVEL     'N'           V3CETP
094600941103     C                   MOVEL     'N'           V3CPKM
094700941103     C                   MOVEL     'N'           V3CPKP
094800021129     C                   MOVEL     'S'           V1CCAM
094900021129     C                   MOVEL     'S'           V1CCAP
095000021129     C                   MOVEL     'S'           V1CSCM
095100021129     C                   MOVEL     'S'           V1CSCP
095200080305     c                   eval      v3cpda = 'N'
095300080305     c                   clear                   v3ctstpda
095400940411     C*
095500940411     C* FLAG USO IN STATISTICA
095600940809     C                   MOVE      V1CPDR        COMCOD            3 0
095700940411     C     COMCOD        IFGE      900
095800950427     C                   MOVEL     'S'           V3CFCM
095900940411     C                   ELSE
096000950427     C                   MOVEL     ' '           V3CFCM
096100940411     C                   ENDIF
096200940411     C*
096300910122     C                   ENDSR
096400030127     C**************************************************************************
096500030127     C*    PULIZIA CAMPI 4°VIDEATA
096600030127     C**************************************************************************
096700030127     C     AZZ4          BEGSR
096800030127     C*
096900050225     C                   MOVE      *BLANKS       V3Cgge
097000050523     C                   MOVE      'P'           V3Ctpm
097100160426     C                   MOVE      '1'           V3Ctur
097200050804     C                   MOVE      *BLANKS       V3Ckm
097300030127     C                   MOVE      *BLANKS       V3CRSC
097400030127     C                   MOVE      *BLANKS       V1CRSF
097500030127     C                   MOVEL     *BLANKS       V3CPDD
097600030127     C                   Z-ADD     *ZEROS        V3CKSC
097700120326     C                   Z-ADD     *ZEROS        saV3CKSC
097800030127     C                   MOVE      *BLANKS       V3DKSC
097900030127     C                   MOVE      *BLANKS       V3CCSF
098000120326     C                   MOVE      *BLANKS       saV3CCSF
098100030127     C                   MOVEL     *BLANKS       V3CFGR
098200030127     C                   CLEAR                   V3CCSF
098300030127     C                   Z-ADD     *ZEROS        V3CPKL
098400030127     C                   Z-ADD     *ZEROS        V3CVLU
098500030128     C                   Z-ADD     *ZEROS        V3Char
098600030128     C                   Z-ADD     *ZEROS        V3Chpa
098700030127     C                   MOVEL     'N'           V3CETM
098800030127     C                   MOVEL     'N'           V3CETP
098900030127     C                   MOVEL     'N'           V3CPKM
099000030127     C                   MOVEL     'N'           V3CPKP
099100030128     C                   MOVEL     'N'           V1CCAM
099200030128     C                   MOVEL     'N'           V1CCAP
099300030128     C                   MOVEL     'N'           V1CSCM
099400030128     C                   MOVEL     'N'           V1CSCP
099500030127     C*
099600030127     C* FLAG USO IN STATISTICA
099700030127     C                   MOVE      V1CPDR        COMCOD            3 0
099800030127     C     COMCOD        IFGE      900
099900030127     C                   MOVEL     'S'           V3CFCM
100000030127     C                   ELSE
100100030127     C                   MOVEL     ' '           V3CFCM
100200030127     C                   ENDIF
100300030127     C*
100400030127     C                   ENDSR
100500940809     C**************************************************************************
100600940809     C*    CARICO DATI NEL SUBFILE
100700940809     C**************************************************************************
100800910116     C     CARICA        BEGSR
100900940809     C*
101000920918     C* PULIZIA SUBFILE
101100940809     C                   SETOFF                                       2021
101200940809     C                   WRITE     LV24C02
101300940809     C                   SETON                                        21
101400920918     C*
101500071205     C                   Z-ADD     0             NRR2
101600120521     C*
101700120517     C*  solo x gli autisti
101800120517     c                   if        v1ctip = 'A'
101900120517     c                   move      'PDA'         PDAAUT
102000120522     c                   else
102100120522     c                   move      'Soc'         PDAAUT
102200120522     c                   end
102300920918     C*
102400021129     C     v1ctip        SETLL     fiapd01L
102500021129     C     v1ctip        READE(N)  fiapd01L                               33
102600021016     C*
102700920921     C     *IN33         DOWEQ     '0'
102800140219     C*forzo uscita dalla lettura se è stato chiesto di vedere tutto l'elenco
102900140219      * ma non c'è più spazio nel sfl
103000140219     c                   if        nrr2 = 9999
103100140219     c                   leave
103200140219     c                   endif
103300920922     C*
103400000726     C* ESCLUDO I RECORDS ANNULLATI
103500021129      * con d24ann = $$ considero anche i record annullati serve per lo
103600020528      * storico delle tariffe annullate
103700021129     c     d24ann        ifne      '$$'
103800000726     C     APDATB        IFNE      ' '
103900000726     C                   GOTO      SOTTO
104000000726     C                   ENDIF
104100020528     C                   ENDIF
104200021016      *
104300021129     C* 15 ON - d24fil <> 0
104400920917     C     *IN15         IFEQ      '1'
104500940809     C                   MOVEL     APDPDR        WFIL              3 0
104600021129     C* CARICO I AUTO/COOP  DELLA FILIALE PASSATA E QUELLI SENZA
104700920917     C*    FILIALE DI APPARTENENZA
104800021129     C     WFIL          IFNE      d24fil
104900940809     C     WFIL          ANDNE     *ZERO
105000920917     C                   GOTO      SOTTO
105100920917     C                   END
105200920922     C                   END
105300021016      *
105400021129RM*  C* PROGRAMMA NON RICHIAMATO: VERIFICO FILIALE AUTO/COOP
105500950314RM*  C     *IN13         IFEQ      *OFF
105600950314RM*  C                   MOVEL     APDPDR        WFIL              3 0
105700950314RM*  C                   SELECT
105800021016      *
105900021129RM*  C* Filiale AUTO/COOP  = 0 e sono un secondo livello carico solo i
106000021129RM*  C* AUTO/COOP  delle filiali della £6
106100950314RM*  C     V1FPAD        WHENEQ    *ZEROS
106200021016      *
106300060203     c     DUTlpo        andne     '1'
106400060203     c     DUTlpo        andne     'S'
106500950314RM*  C     WFIL          LOOKUP    L6                                     31
106600950314RM*  C     *IN31         CABEQ     *OFF          SOTTO
106700021016      *
106800021016      * se 1° livello deve vedere tutta l'area
106900021016RM*  C     V1FPAD        WHENEQ    *ZEROS
107000060203     c     DUTlpo        andeq     '1'
107100021016RM*  C     WFIL          LOOKUP    L1                                     31
107200021016RM*  C     *IN31         CABEQ     *OFF          SOTTO
107300021016      *
107400021016      * se è la sede deve vedere tutto
107500021016RM*  C     V1FPAD        WHENEQ    *ZEROS
107600060203     c     DUTlpo        andeq     'S'
107700021016      *
107800021129RM*  C* Filiale AUTO/COOP  diversa da 0: carico solo i AUTO/COOP  della
107900950314RM*  C* filiale richiesta e quelli senza filiale appartenenza
108000950314RM*  C     V1FPAD        WHENNE    *ZEROS
108100950314RM*  C     WFIL          IFNE      V1FPAD
108200950314RM*  C     WFIL          ANDNE     *ZERO
108300950314RM*  C                   GOTO      SOTTO
108400950314RM*  C                   END
108500021016      *
108600950314RM*  C                   ENDSL
108700021016      *
108800950314RM*  C                   END
108900920917     C*
109000021129     C* 15 OFF- d24fil = 0
109100940809     C                   MOVEL     *BLANKS       VIDSEL
109200940809     C                   MOVEL     APDPDR        VSCPDR
109300940809     C                   MOVEL     APDRSC        VSCRSC
109400110304     C                   MOVEL     APDptp        VSCptp
109500000427      *
109600050519     c                   movel     apdflr        dapdflr
109700120522      *
109800120522      * Tipo Messaggeria o Parcel
109900050519     c                   if        §apdtpm='M'
110000050519     c                   movel     'Mes'         vsctpm
110100050519     c                   else
110200050523     c                   movel     'Prc'         vsctpm
110300050519     c                   endif
110400160426      * Turno Parcel
110500160426     c                   clear                   vsctur
110600160426     c                   if        §apdtpm=' '
110700160426     c                   if        §apdturno = ' '
110800160426     c                   movel     '1'           vsctur
110900160426     c                   else
111000160426     c                   movel     '2'           vsctur
111100160426     c                   endif
111200160426     c                   endif
111300000427      *
111400120217     C                   move      APDcsf        VSocope
111500120522     C  n23              move      APDCSF        vscpdaSOC
111600120217     C                   Z-ADD     APDKSC        VSCKSC
111700940809     C                   Z-ADD     APDPKL        VSCPKL
111800120521     c                   Z-ADD     APDVLU        VSCVLU
111900120515      **
112000120515      **  si imposta dal filler se ha PDA o test PDA
112100120521      **    all'inizio non considera l'ordinamento x PDA o NO PDA
112200120517     c                   z-add     0             h1ord
112300120517
112400120517     c                   if        v1ctip='A'
112500120515     c                   movel     apdflr        dapdflr
112600120522     c                   eval      %subst(vscPDAsoc:1:1) = §apdpda
112700120517      **
112800120515     c                   if        §apdpda  = *blanks
112900120517      **  per ordinare sempre quelle senza PDA davanti agli altri
113000120522     c                   eval      %subst(vscPDAsoc:1:1) = 'N'
113100120515     c                   end
113200120522     c                   eval      %subst(vscPDAsoc:3:1) = §apdtstpda
113300120517     c                   end
113400120515      **
113500120216      * verifico se trazionista per elenco autisti A/D lo segnalo
113600120216      * mettendo in evidenza il codice aut e un flag occulto con 'T' sulla riga
113700120217     c                   setoff                                       25
113800120216     c                   exsr      veriftraz
113900140219      *
114000940809     C                   ADD       1             NRR2
114100940809     C  N20              SETON                                        20
114200940809     C                   WRITE     LV24S02
114300920917     C*
114400920917     C     SOTTO         TAG
114500021202     C     V1CTIP        READE(N)  fiapd01L                               33
114600910116     C                   END
114700940809     C                   Z-ADD     1             REC2
114800940809     C*
114900120517     C*  Ordina per Codice Cliente con prioritariamente i PDA ='N'
115000120517     C*  davanti a tutto.
115100120521     c                   eval         Ordinamento   ='1'
115200120522     c                   eval         Ordina_PDA    ='S'
115300120521     c                   eval         Ordina_COD    ='S'
115400120521      *
115500120517     C                   Z-ADD     nrr2          STMREC1
115600120521     c                   exsr      Ordina
115700120517     C*
115800910116     C                   ENDSR
115900940809     C**************************************************************************
116000940809     C*    SUBROUTINE 3° VIDEATA
116100940809     C**************************************************************************
116200091106     C     sraut         BEGSR
116300091106     c                   setoff                                       44
116400131008     c                   clear                   tibs42ds
116500131008     c                   movel     v1fpad        i42pge
116600131008     c                   call      'TIBS42R'
116700131008     c                   parm                    tibs42ds
116800131008     c                   movel     o42uni        uteautds
116900091106     c* aut: utente non abilitato alla manutenzione tariffe
117000091106     c                   if        §autmtc <>'S' and v1ctip = 'A'
117100091106     c* in inserimento anagrafica, errore
117200091106     c                   if        not %found(fiapd01l)
117300091106     C                   MOVEL     MSG(25)       V1CMSG
117400091106     C                   SETON                                        28
117500091106     c                   else
117600091106     c* in modifica anagrafica, proteggo il codice fornmitore
117700091106     c                   seton                                        44
117800091106     c                   end
117900091106     c                   end
118000100421     c* coop: utente non abilitato alla manutenzione tariffe
118100100421     c                   if        §autmtcc <>'S' and v1ctip = 'C'
118200100421     c* in inserimento anagrafica, errore
118300100421     c                   if        not %found(fiapd01l)
118400100421     C                   MOVEL     MSG(25)       V1CMSG
118500100421     C                   SETON                                        28
118600100421     c                   else
118700100421     c* in modifica anagrafica, proteggo il codice fornmitore
118800100421     c                   seton                                        44
118900100421     c                   end
119000100421     c                   end
119100091106     c* aff/defl: utente non abilitato alla manutenzione tariffe
119200091106     c                   if        §affmtc <>'S' and v1ctip = 'D'
119300091106     c* in inserimento anagrafica, errore
119400091106     c                   if        not %found(fiapd01l)
119500091106     C                   MOVEL     MSG(25)       V1CMSG
119600091106     C                   SETON                                        28
119700091106     c                   else
119800091106     c* in modifica anagrafica, proteggo il codice fornmitore
119900091106     c                   seton                                        44
120000091106     c                   end
120100091106     c                   end
120200091106     C                   ENDSR
120300091106     C**************************************************************************
120400091106     C*    SUBROUTINE 3° VIDEATA
120500091106     C**************************************************************************
120600091106     C     VIDEO3        BEGSR
120700940809     C*
120800090706     c* per venezia visualizzo il tipo mezzo per impostare BARCA
120900090706     c     v1fpad        comp      058                                6262
121000000726     C                   SETOFF                                       111241
121100161108     C                   SETOFF                                         4546
121200050519     c* verifico se filiale abilitata al parcel + messaggerie
121300050519     c                   exsr      srtpm
121400920921     C*
121500130729     c                   clear                   v3ctel
121501171211     c                   clear                   v3ccel
121502171212     c                   setoff                                       29
121600940809     C                   MOVEL     V1CPDR        KPDR
121700021129     C     Kapd          CHAIN(N)  fiapd01L                           32
121800130726     c                   movel     kpdr          kpdr3             3 0
121900130726     C     Kapd4         CHAIN(N)  fiapd41L
122000130726     c                   clear                   dapd4tel
122100130726     c                   if        %found(fiapd41l)
122200130726     c                   movel     apd4uni       dapd4tel
122300130729     c                   movel     §apd4tel      v3ctel
122301171205     c                   if        §apd4cel <> *blank
122302171205     c                   movel     §apd4cel      v3ccel
122303171205     c                   seton                                        29
122304171212     c                   else
122305171212     c                   exsr      CercaCel
122306171212     c                   if        onumcel <> *blank and Oesito = '1'
122307171212     c                   movel     onumcel       v3ccel
122308171212     c                   seton                                        29
122309171212     c                   endif
122310171205     c                   endif
122400130726     c                   endif
122500091106     c* verifico se abilitato alla manutenzione tariffe per poter inserire
122600091106     c* l'anagrafico o modi
122700091106     c                   exsr      sraut
122800091106     c   28              goto      for02
122900091106     c* inserimento
123000931130    1C     *IN32         IFEQ      '1'
123100920921     C                   SETON                                        11
123200920922     C                   EXSR      AZZ
123300920922     C*
123400931130   X1C                   ELSE
123500920921     C                   SETON                                        12
123600000726     C     APDATB        IFNE      ' '
123700000726     C                   SETON                                        41
123800000726     C                   ENDIF
123900940809     C                   MOVEL     APDRSC        V3CRSC
124000021129     C                   MOVEL     APDRSF        V1CRSF
124100050518     c                   movel     apdflr        dapdflr
124200050804     c                   move      §apdkm        v3ckm
124300050519     c                   movel     §apdtpm       v3ctpm
124400160426     c                   movel     §apdturno     v3ctur
124500090706     c                   movel     §apdmezzo     v3cmezzo
124600161025      * sotto VPO   solo x Autotrasportatori
124700161025     c                   if        *in23 and *in52
124800161025     c                   move      §apdspec      v3cspec
124900161003     c                   move      §apdats       v3cats
125000161003     c                   clear                   v3catsd
125100161003     c                   if        v3cats <> *blank
125200161003     c                   move      'ATS'         tbecod
125300161025     c                   movel(p)  v3cats        tbeke1
125400161025      *  decod. Territtorio
125500161003     c     ktbe          chain     tntbe01l
125600161003     c                   if        %found(tntbe01l)
125700161003     c                   movel     tbeuni        dats
125800161027     c                   movel     d§atsdes      v3catsd
125900161025     c                   endif
126000161003     c                   endif
126100161108      **
126200161108      * sotto VPO   solo x Autotrasportatori
126300161108     c                   move      §apdaat       v3caat
126400161108     c                   clear                   v3daat
126500161108     c                   if        v3caat <> *blank
126600161108     c                   move      'AAT'         tbecod
126700161108     c                   movel(p)  v3caat        tbeke1
126800161108      *  decod. Territtorio
126900161108     c     ktbe          chain     tntbe01l
127000161108     c                   if        %found(tntbe01l)
127100161108     c                   movel     tbeuni        daat
127200161108     c                   movel     d§aatdes      v3daat
127300161108     c                   endif
127400161108     c                   endif
127500161108     c                   end
127600161025      * sotto VPO
127700110303     c                   if        apdptp  <> *blank
127800110303     c                   move      apdptp        v3cbans
127900110228     c                   else
128000110228     c                   move      *zeros        v3cbans
128100110228     c                   endif
128200050523     c                   if        §apdtpm = *blanks
128300050523     c                   eval      v3ctpm = 'P'
128400050523     c                   end
128500160427     c                   if        §apdtpm = *blanks
128600160426     c                   if        §apdturno = ' '
128700160426     c                   eval      v3ctur = '1'
128800160426     c                   else
128900160426     c                   eval      v3ctur = '2'
129000160427     c                   end
129100160426     c                   end
129200070723     c                   movel     §apdpda       v3cpda
129300070723     c                   if        §apdpda  = *blanks
129400070723     c                   eval      v3cpda = 'N'
129500070122     c                   end
129600080206     c                   movel     §apdtstpda    v3ctstpda
129700080911     c                   if        %subst(apdflr:9:1)= *blanks
129800080911     c                   clear                   v3cmcp
129900080911     c                   else
130000080911     c                   eval      v3cmcp= §apdmcp
130100080911     c                   end
130200080207     c* se procedura attiva e aut in test avviso
130300080212     c                   clear                   msgv
130400080212     c                   if        *in96
130500080207     C                   if        (v3ctstpda ='E' and
130600080211     C                             (§OGPDAORM ='S' OR §OGPDACON='S'))
130700080207     C                             or (v3ctstpda ='C' and
130800080207     C                             §OGPDACON='S')
130900080207     C                             or (v3ctstpda ='O' and
131000080207     C                             §OGPDAORM='S')
131100080207     C                   eval      msgv= 'ATT. PDA attiv.'
131200080207     C                   ENDIF
131300080213     C                   ENDIF
131400050518     C                   TESTN                   §apdgcd              89
131500050518     C   89              MOVE      §apdgcd       v3cgge
131600050225     C  n89              MOVE      *zeros        v3cgge
131700000502      *
131800940809     C                   MOVEL     APDKSC        V3CKSC
131900000808     C                   MOVEL     APDCSF        V3CCSF
132000120326     C                   MOVEL     APDKSC        saV3CKSC
132100120326     C                   MOVEL     APDCSF        saV3CCSF
132200940809     C                   MOVEL     APDPKL        V3CPKL
132300140728     C                   MOVEL     APDVLU        V3CVLU
132400940809     C                   MOVEL     APDPDD        V3CPDD
132500940809     C                   MOVEL     APDFGR        V3CFGR
132600940809     C                   MOVEL     APDETM        V3CETM
132700940809     C                   MOVEL     APDETP        V3CETP
132800940809     C                   MOVEL     APDPKM        V3CPKM
132900940809     C                   MOVEL     APDPKP        V3CPKP
133000161025     C                   MOVEL     APDSNU        V3CSNU
133100941024     C                   MOVEL     APDFCM        V3CFCM
133200021129     C                   MOVEL     APDCAM        V1CCAM
133300021129     C                   MOVEL     APDCAP        V1CCAP
133400021129     C                   MOVEL     APDSCM        V1CSCM
133500021129     C                   MOVEL     APDSCP        V1CSCP
133600030218      *
133700030218      *  Per le Coop il Nastro è oscurato e sempre a 0
133800030218     c                   if        v1ctip = 'A'
133900030128     c                   z-add     apdhpa        v3chpa
134000030128     c                   z-add     apdhar        v3char
134100030218     c                   else
134200030218     c                   z-add     0             v3chpa
134300030218     c                   z-add     0             v3char
134400030218     c                   end
134500030218      *
134600940809     C                   MOVEL     *BLANKS       V3DKSC
134700920922     C*
134800000808     C* DECODIFICO CODICE FORNITORE
134900940809    2C     V3CKSC        IFNE      0
135000000808     C     V3CCSF        ANDNE     *BLANKS
135100090330     c                   exsr      srrco
135200931130    2C                   END
135300920923     C*
135400931130    1C                   END
135500060515     C*
135600060515     C* SE L'UTENTE NON E' "EDP..." NON E' POSSIBILE MANUTENZIONARE ALCUNI CAMPI
135700060515     C                   MOVEL     KNMUS         $TRE              3
135800060515     C     $TRE          COMP      'EDP'                              1414
135900920921     C*
136000940809     C                   MOVEL     'S'           WRIEM
136100920921     C     FOR02         TAG
136200940809     C     WRIEM         IFEQ      'S'
136300940809     C                   WRITE     LV24T01
136400940809     C                   MOVEL     *BLANKS       WRIEM
136500940809     C                   ENDIF
136600081022      *
136700081022      * protegge x il "998" il flag di "Merce portata a Magazzino"
136800081022     c                   move      V1CPDR        cod998            3
136900081022     C                   SETOFF                                       59
137000081022     c                   if        cod998 ='998'
137100081022     C                   SETOn                                        59
137200081022     c                   end
137300081022      *
137400940809     C                   EXFMT     LV24D03
137500940809     C*
137600940809     C                   SETOFF                                       285356
137700940809     C                   SETOFF                                       400102
137800940809     C                   SETOFF                                       030405
137900940809     C                   SETOFF                                       060708
138000031204     C                   SETOFF                                       091054
138100010206     C                   SETOFF                                       919293
138200080206     C                   SETOFF                                       949586
138300140728     C                   SETOFF                                       871847
138400161003     C                   SETOFF                                       482442
138500161108     C                   SETOFF                                       4546
138600170102     C                   SETOFF                                       98
138700940809     C*
138800920921     C* CMD3 - FINE LAVORO
138900090330     C* CMD12 - RITORNO
139000090330     c                   if        *inkc or *inkl
139100090331     c                   if        flgcmt = *on
139200090331     c                   rolbk
139300090331     c                   end
139400090330     C                   GOTO      FINE2
139500090330     c                   end
139501170510     C* F02 Variazioni
139502170510if  1C                   IF        *inkb
139503170510     c                   movel     v1ctip        ilv37tip
139504170510     c                   movel     v1cpdr        ilv37pdr
139505170510     c                   movel     kpjbu         kpjbusav
139506170510     c                   movel     fnlv37ds      kpjbu
139507170510     c                   call      'FNLV37R'
139508170510     c                   parm                    kpjba
139509170510     c                   movel     kpjbusav      kpjbu
139510170510     c                   endif
139600091106     c* verifico se abilitato alla manutenzione tariffe per poter inserire
139700091106     c* l'anagrafico o modi
139800091106     c                   exsr      sraut
139900091106     c   28              goto      for02
140000920921     C*
140100920921     C* CMD16 - ANNULLAMENTO
140200920923     C   KQ              DO
140300071205     C* impossibile annullate i codici "tecnici"
140400071205     c                   if        v1ctip = 'A' and
140500071205     c                             (v1cpad = '0999' or
140600071205     c                             v1cpad = '0998' or
140700071205     c                             v1cpad = '9876')
140800071205     C                   MOVEL     MSG(10)       V1CMSG
140900071205     C                   SETON                                        28
141000071205     C                   GOTO      FOR02
141100071205     c                   end
141200950127     C*
141300120515     C*----
141400120515     C*    Se ci sono delle distinte aperte sul codice Autista
141500120515     C*      non può essere annullato.
141600120515     c                   clear                   esito_DST         1
141700120515      * solo x la città controlla le distinte
141800120515     c                   if        v1ctip = 'A'
141900120515      *** su SETRAS
142000120515     c                   if        (%subst(knsif:7:1) <>'P')
142100120515     C/EXEC SQL
142200120515     C+ SELECT '1' into :esito_dst
142300120515     C+    FROM filtra201/fidst00f WHERE DSTFCF =' ' and
142400120515     C+     DSTATB = ' ' and DSTNPG =4 and DSTPDR = :apdPDR
142500120515     C+ FETCH first row only
142600120515     C/END-EXEC
142700120515     c                   end
142800120515     **
142900120515      *** su 888
143000120515      *   sono in test quindi deve puntare sulla libreria di test
143100120515     c                   if        (%subst(knsif:7:1) = 'P')
143200120515     C/EXEC SQL
143300120515     C+ SELECT '1' into :esito_dst
143400120515     C+    FROM filtraprd/fidst00f WHERE DSTFCF =' ' and
143500120515     C+     DSTATB = ' ' and DSTNPG =4 and DSTPDR = :apdPDR
143600120515     C+ FETCH first row only
143700120515     C/END-EXEC
143800120515     c                   end
143900120515      *   se trovato non deve permettere l'annullamento
144000120515     c                   if        SQLcod = 0 and esito_DST ='1'
144100120515     C                   MOVEL     MSG(28)       V1CMSG
144200120515     C                   SETON                                        28
144300120515     C                   GOTO      FOR02
144400120515     c                   end
144500120515      *
144600120515     c                   end
144700120515     C*----
144800120515     C*
144900030128     C* IMPOSSIBILE ANNULLARE autotrasport. SE ESISTONO TARIFFE
145000011120     C     KPDR          SETLL     fiFGT01L
145100011120     C     KPDR          READE     fiFGT01L                               99
145200961128     C     *IN99         DOWEQ     *OFF
145300961128     C     FGTATB        IFEQ      ' '
145400090610     C     FGTsml        andeq     ' '
145500040421     c     v1ctip        andeq     'A'
145600040421     c                   if        fgttsr = 'C' or
145700040421     c                             fgttsr = 'R' or
145800040421     c                             fgttsr = 'G'
145900090610     c* se tariffa non ricevuta copia firmata non annullo il codice
146000090610     c                   if        fgtdcn = 0 or
146100090610     c                             fgtdst >= wdate
146200961128     C                   MOVEL     MSG(13)       V1CMSG
146300961128     C                   SETON                                        28
146400961128     C                   GOTO      FOR02
146500040421     C                   end
146600090610     C                   end
146700021129     C                   ENDIF
146800011120     C     KPDR          READE     fiFGT01L                               99
146900961128     C                   ENDDO
147000940905     C*
147100021129     C     Kapd          CHAIN     fiapd01L                           3298
147200000726     C     *IN98         IFEQ      *OFF
147300000726     C  N32              MOVE      'A'           APDATB
147400021129     C  N32              CLEAR                   APDDTR
147500021129     C  N32              CLEAR                   APDFTR
147600021129     C  N32              UPDATE    fiapd000                             98
147700130731     C  n32Kapd4         CHAIN     fiapd41L                           3298
147800130731     c                   if        %found(fiapd41l)
147900130731     c                   move      'A'           apd4atb
148000130731     c                   update    fiapd400
148100130731     c                   endif
148200170508      * preparazione dati richiamo PGM variazioni per ANNULLAMENTO
148300170508     c                   clear                   fnlv36ds
148400170508     c                   clear                   fiapdb
148500170510     c                   clear                   fiapda
148600170508     c                   clear                   fiapd4b
148700170510     c                   clear                   fiapd4A
148701170524     c                   eval      ILV36tip  = v1ctip
148800170508     c                   eval      ILV36PRU  = knmus
148900170508     c                   eval      ILV36NOJ  = knmeb
149000170508     c                   eval      ILV36PGM  = 'FNLV24R'
149100170508     c                   eval      ILV36CTA  = 'A'
149200170508     c                   eval      fiapdb =  fiapdds
149300170508      *richiama pgm registrazione variazioni
149400170508     c                   call      'FNLV36R'
149500170508     c                   parm                    fnlv36ds
149600170508     c                   parm                    fiapdb
149700170510     c                   parm                    fiapda
149800170508     c                   parm                    fiapd4b
149900170510     c                   parm                    fiapd4A
150000060925     c* annullo anche l'eventuale estensione
150100060925     c                   if        not *in32 and not *in98
150200060925     c                   exsr      srapd2
150300070208     c                   if        *in28
150400070208     c                   if        flgcmt = *on
150500070208     c                   rolbk
150600070208     c                   end
150700070208     C                   GOTO      FOR02
150800060925     c                   end
150900070208     c                   end
151000070208     c                   if        not *in98 and flgcmt = *on
151100070208     c                   commit
151200070208     c                   end
151300000726     C  N98              SETON                                        42
151400000726     C  N98              GOTO      FINE2
151500000726     C                   ENDIF
151600000726     C     *IN98         IFEQ      *ON
151700000726     C                   MOVEL     MSG(4)        V1CMSG
151800000726     C                   SETON                                        28
151900000726     C                   GOTO      FOR02
152000000726     C                   ENDIF
152100920923     C                   END
152200000726     C*
152300920921     C* CONTROLLI
152400920921     C                   EXSR      CONTRS
152500000726      *
152600000726     C     *IN90         IFEQ      *ON
152700000726     C                   GOTO      FOR02
152800000726     C                   ELSE
152900000726     C     *INKE         IFEQ      *OFF
153000000726     C     *INKF         ANDEQ     *OFF
153100000726     C                   GOTO      FOR02
153200000726     C                   ENDIF
153300060927     C                   ENDIF
153400920921     C*
153500090331     c                   exsr      srcdf1
153600090331     c                   if        d27err <> ' '
153700090331     c                   goto      for02
153800090331     c                   end
153900090331     c*
154000920921     C                   EXSR      SCRIVI
154100060925     c* estensione anagrafico aut.
154200070115     c  n98              exsr      srapd2
154300090331     c* aggiorna scadenza tariffa per cambio p.iva
154400070208     c                   if        *in28
154500070208     c                   if        flgcmt = *on
154600070208     c                   rolbk
154700070208     c                   end
154800070208     C                   GOTO      FOR02
154900070208     c                   end
155000070208     c                   if        not *in98 and flgcmt = *on
155100070208     c                   commit
155200070208     c                   end
155300931130     C* 98 ON - WRITE UPDATE O CHAIN NON ANDATE A BUON FINE
155400931130     C   98              GOTO      FOR02
155500920921     C*
155600920921     C     FINE2         ENDSR
155601171212     c*_____________________________________________________________
155602171212     c     CercaCel      begsr
155603171212     c*_____________________________________________________________
155604171212     c                   movel     kpjbu         kpjbusav
155605171212     c                   clear                   param
155606171212     c                   z-add     kpdr          icodaut
155607171212     c                   movel     param         kpjbu
155608171212     c                   call      'TRULCFAUR'
155609171212     c                   parm                    kpjba
155610171212     c                   movel     kpjbu         param
155611171212     c                   movel     kpjbusav      kpjbu
155612171212     c                   endsr
155700940809     C**************************************************************************
155800060925     C*    estensione anagrafico autotrasp.
155900940809     C**************************************************************************
156000060925     C     srapd2        BEGSR
156100070723     c*
156200070723     c                   if        §OGTLLS<> ' ' and
156300070723     c                             (*inke or *inkq)
156400070723     c     kapd21        setll     fiapd22l
156500070723     c                   do        *hival
156600070723     c     kapd21        reade     fiapd22l
156700070723     c                   if        %eof(fiapd22l)
156800070723     c                   leave
156900070723     c                   end
157000070723     c                   if        *inkq
157100070723     c                   eval      apd2atb = 'A'
157200070723     c                   else
157300070723     c                   clear                   apd2atb
157400070723     c                   end
157500070723     c                   update    fiapd200
157600070723     c* pianificazione consegne
157700070723     c     apd2nras      setll     fiapd31l
157800070723     c                   do        *hival
157900070723     c     apd2nras      reade     fiapd31l
158000070723     c                   if        %eof(fiapd31l)
158100070723     c                   leave
158200070723     c                   end
158300070723     c                   if        *inkq
158400070723     c                   eval      apd3atb = 'A'
158500070723     c                   else
158600070723     c                   clear                   apd3atb
158700070723     c                   end
158800070723     c                   update    fiapd300
158900070723     c                   enddo
159000070723     c* pianificazione ritiri
159100070723     c     apd2nras      setll     fiapd51l
159200070723     c                   do        *hival
159300070723     c     apd2nras      reade     fiapd51l
159400070723     c                   if        %eof(fiapd51l)
159500070723     c                   leave
159600070723     c                   end
159700070723     c                   if        *inkq
159800070723     c                   eval      apd5atb = 'A'
159900070723     c                   else
160000070723     c                   clear                   apd5atb
160100070723     c                   end
160200070723     c                   update    fiapd500
160300070723     c                   enddo
160400070723     c                   enddo
160500070723     c                   end
160600070208     C                   ENDSR
160700070208     C**************************************************************************
160800070208     C*    CONTROLLo se filiale è abilitata a parcel + messaggerie
160900070208     C**************************************************************************
161000070208     C     srtpm         BEGSR
161100050524     c* se richiamato
161200050524     c                   if        *in13
161300050524     c                   move      d24fil        orgfil
161400050524     c                   else
161500050524     c                   move      v1fpad        orgfil
161600050524     c                   end
161700050519     c                   setoff                                       16
161800050519     c     orgfil        chain     azorg01l
161900050519     c                   if        %found(azorg01l)
162000070723     C                   MOVEL     ORGDe8        og148
162100070108     C                   MOVEL     ORGDE6        og146
162200070723     C                   MOVEL     ORGDF0        og150
162300050519     c     §OGPKM        comp      'S'                                    16
162400080212     c                   if        §OGPDAorm = ' ' and §OGPDACON = ' '
162500080212     c                   setoff                                       96
162600080212     c                   else
162700080212     c                   seton                                        96
162800080212     c                   end
162801170516     c                   if        vediturno = *blank
162900160426     c     §OGturno      comp      'S'                                    27
162901170510     c                   endif
163000070313     c                   end
163100050519     C                   ENDSR
163200050519     C**************************************************************************
163300050519     C*    CONTROLLI 3° VIDEATA
163400050519     C**************************************************************************
163500050519     C     CONTRS        BEGSR
163600940809     C*
163700920921     C                   SETOFF                                       90
163800120308     C*-
163900940809     C                   CLEAR                   V3DKSC
164000120308     C*-
164100090401     c* verifico se aut disaccreditato
164200090401     c*                  clear                   tradfi
164300090401     c*    ktra          chain     aitra03l
164400090401     c*                  if        %found(aitra03l) and tradfi<> 0
164500090401     c*                  eval      msgv='Autista disaccreditato'
164600090401     C*                  SETON                                        2890
164700090401     C*                  GOTO      ENDCON
164800090401    1C*                  END
164900140219     C*** DESCRIZIONE autotrasportatore OBBLIGATORIA
165000140219     c                   move      v1cpdr        pdr4              4 0
165100160405     c                   if        pdr4 >= 9500 and pdr4 <= 9509 and
165200140219     c                             %subst(knmus:1:3) <>'EDP'
165300140219     C                   MOVEL     MSG(31)       V1CMSG
165400140219     C                   SETON                                        28  90
165500140219     C                   GOTO      ENDCON
165600140219    1C                   END
165700030128     C*** DESCRIZIONE autotrasportatore OBBLIGATORIA
165800940809    1C     V3CRSC        IFEQ      *BLANKS
165900940809     C                   MOVEL     MSG(3)        V1CMSG
166000940809     C                   SETON                                        285390
166100920918     C                   GOTO      ENDCON
166200931130    1C                   END
166300021129     C* descrizione per fatturaz.
166400031204     c                   if        v1crsf = *blanks
166500031204     c*                  movel(p)  v3crsc        v1crsf
166600031204     c                   seton                                        542890
166700031204     C                   MOVEL     MSG(3)        V1CMSG
166800031204     C                   GOTO      ENDCON
166900031204     c                   end
167000931130     C*
167100931130     C*** CODICE FORNITORE  E  SISTEMA INFORMATIVO
167200020412     C* obbligatori solo per non dipendenti
167300020328    1C     V3CKSC        IFeq      0
167400020412    1C     V3Cpdd        andne     'S'
167500020328     C                   MOVEL     MSG(18)       V1CMSG
167600020328     C                   SETON                                        285690
167700020328     C                   GOTO      ENDCON
167800020328    1C                   ENDIF
167900020328    1C     V3CCSF        IFeq      *BLANKS
168000020412    1C     V3Cpdd        andne     'S'
168100020328     C                   MOVEL     MSG(19)       V1CMSG
168200020328     C                   SETON                                        284090
168300020328     C                   GOTO      ENDCON
168400020328    1C                   ENDIF
168500020328      *
168600940809    1C     V3CKSC        IFNE      0
168700940809     C     V3CCSF        ANDEQ     *BLANKS
168800940809     C                   MOVEL     MSG(8)        V1CMSG
168900940809     C                   SETON                                        284090
169000931130     C                   GOTO      ENDCON
169100931130    1C                   ENDIF
169200931130     C*
169300120517ab   C*-----
169400120625ab   C* obbligatori solo se NON esclusi da Fatturazione
169500120625ab  1C     V3Cpdd        ifNE      'S'
169600120517ab   C*  deve controllare
169700120517ab   C*   che la società sia quella operativa della filiale
169800120517ab   C                   exsr      CHK_SOC_OPE
169900120517ab   c                   if        v3cCSF <> idsocieta
170000120517ab   c                   seton                                        289040
170100120517ab   C                   MOVEL     MSG(21)       V1CMSG
170200120517     C                   GOTO      ENDCON
170300120517ab   C                   end
170400140728      * merce portata a magazzino
170500140728     c                   if        v3cfcm = 'S'
170600140728     c                   seton                                        289047
170700140728     C                   MOVEL     MSG(33)       V1CMSG
170800140728     C                   GOTO      ENDCON
170900140728     C                   end
171000140728      *controlli per padroncino escluso da fatturazione
171100140728     c                   else
171200140728     c                   if        v3cetm = 'S' or
171300140728     c                             v3cetp = 'S' or
171400140728     c                             v3cpkm = 'S' or
171500140728     c                             v3cpkp = 'S' or
171600140728     c                             v1ccam = 'S' or
171700140728     c                             v1ccap = 'S' or
171800140728     c                             v1cscm = 'S' or
171900140728     c                             v1cscp = 'S'
172000140804     c                   seton                                        289048
172100140728     C                   MOVEL     MSG(32)       V1CMSG
172200140728     C                   GOTO      ENDCON
172300140728     C                   end
172400140728      * merce portata a magazzino
172500140728     c                   if        v3cfcm <> 'S'
172600140728     c                   seton                                        289047
172700140728     C                   MOVEL     MSG(34)       V1CMSG
172800140728     C                   GOTO      ENDCON
172900140728     C                   end
173000171102     C* società e codice fornitore non devono essere presenti se escluso da FATT
173001171102     c                   if        v3cksc <> 0 or
173002171102     c                             v3ccsf <> *blank
173003171102     c                   seton                                        285640
173004171102     C                   MOVEL     MSG(40)       V1CMSG
173005171102     C                   GOTO      ENDCON
173006171102     C                   end
173007171102     C*
173100140728     C                   end
173200140728     C*-----
173300940809    1C     V3CKSC        IFEQ      0
173400940809     C     V3CCSF        ANDNE     *BLANKS
173500940809     C                   MOVEL     MSG(7)        V1CMSG
173600940809     C                   SETON                                        285690
173700931130     C                   GOTO      ENDCON
173800931130    1C                   ENDIF
173900090330     C*** CONTROLLO SE PRESENTE IN proj
174000000808     C     V3CKSC        IFNE      0
174100090330     c                   exsr      srrco
174200000808     C   90              GOTO      ENDCON
174300000808     C                   ENDIF
174400120308      **--
174500071203     c* controllo il fornitore di fatturazione
174600071203     c                   exsr      srcdf
174700071203     C   90              GOTO      ENDCON
174800141106     c* controllo per numero di telfono
174900141106     c                   if        v3ctel <> *blank
175000141106     c                   exsr      chktel
175100141118     c                   eval      leng=%len(%trim(v3ctel))
175200150120     c                   if        v1err <> 'N'
175300150120     c*                             and leng < 9
175400141118     C                   SETON                                        9028
175500141106     C                   MOVEL     MSG(35)       V1CMSG
175600141106     C                   GOTO      ENDCON
175700141106     c                   endif
175800141106     c                   endif
175900161110      **--
176000161110     C*controlli territorio servito
176100161110      * sotto VPO solo x autotrasportatori
176200161110     c                   if        *in23 and *in52
176300161110     c                   clear                   v3catsd
176400161110     c                   if        v3cats <> *blank
176500161110     c                   exsr      checkats
176600161110     C                   end
176700161110     c                   if        t02err <> *blank  OR
176800161110     c                             v3cats  = *blank
176900161110     c                   seton                                        289046
177000161110     C                   MOVEL     MSG(38)       V1CMSG
177100161110     C                   GOTO      ENDCON
177200161110     C                   end
177300161110     C                   endif
177400161110      **--
177500161110     C*controlli tipo attività
177600161110      * sotto VPO solo x autotrasportatori
177700161110     c                   if        *in23 and *in52
177800161110     c                   clear                   v3daat
177900161110     c                   if        v3caat <> *blank
178000161110     c                   exsr      checkaat
178100161110     C                   end
178200161110     c                   if        t02err <> *blank  OR
178300161110     c                             v3caat  = *blank
178400161110     c                   seton                                        289045
178500161110     C                   MOVEL     MSG(39)       V1CMSG
178600161110     C                   GOTO      ENDCON
178700161110     C                   end
178800161110     C                   endif
178900161110      **--
179000080212     c* controllo flag dati per PDA se attivo
179100080212     c                   if        *in96
179200080206     C                   if        (v3cpda = 'O' and
179300080206     c                             §OGPDAORM = ' ') or
179400080206     C                             (v3cpda = 'C' and
179500080206     c                             §OGPDACON = ' ') or
179600080206     C                             (v3cpda = 'E' and
179700080206     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
179800080206     C                   MOVEL     MSG(22)       V1CMSG
179900080206     c                   select
180000080206     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
180100080206     c                   eval      v1cmsg = %trim(v1cmsg)+' CONSEGNE'
180200080206     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
180300080206     c                   eval      v1cmsg = %trim(v1cmsg) + ' ORM'
180400080206     c                   endsl
180500080206     C                   SETON                                        908628
180600080206     C                   GOTO      ENDCON
180700080206     C                   ENDIF
180800161110      **--
180900080206     c* controllo flag test per PDA
181000080206     C                   if        (v3ctstpda = 'O' and
181100080206     c                             §OGPDAORM = ' ') or
181200080206     C                             (v3ctstpda = 'C' and
181300080206     c                             §OGPDACON = ' ') or
181400080206     C                             (v3ctstpda = 'E' and
181500080206     c                             (§OGPDACON = ' ' or §OGPDAORM = ' '))
181600080206     C                   MOVEL     MSG(22)       V1CMSG
181700080206     c                   select
181800080206     c                   when      §OGPDACON = ' ' and §OGPDAORM <> ' '
181900080206     c                   eval      v1cmsg = %trim(v1cmsg) + ' CONSEGNE'
182000080206     c                   when      §OGPDACON <>' ' and §OGPDAORM = ' '
182100080206     c                   eval      v1cmsg = %trim(v1cmsg) + ' ORM'
182200080206     c                   endsl
182300080206     C                   SETON                                        908728
182400080206     C                   GOTO      ENDCON
182500080206     C                   ENDIF
182600080215     c* controllo la congruenza dei valori impostati nel test x pda e
182700080215     c* e nei dati x pda
182800080215     C                   if        v3ctstpda <>' '
182900080215     C                   if        (v3cpda = 'O' and
183000080215     C                             v3ctstpda <>'O') or
183100080215     C                             (v3cpda = 'C' and
183200080215     C                             v3ctstpda <>'C') or
183300080215     C                             v3cpda = 'N'
183400080215     C                   MOVEL     MSG(23)       V1CMSG
183500080215     C                   SETON                                        908728
183600080215     C                   GOTO      ENDCON
183700080215     C                   ENDIF
183800080215     C                   ENDIF
183900080207     c* se procedura attiva e aut. in test avviso
184000080212     c                   clear                   msgv
184100080207     C                   if        (v3ctstpda ='E' and
184200080211     C                             (§OGPDAORM ='S' or §OGPDACON='S'))
184300080207     C                             or (v3ctstpda ='C' and
184400080207     C                             §OGPDACON='S')
184500080207     C                             or (v3ctstpda ='O' and
184600080207     C                             §OGPDAORM='S')
184700080207     C                   eval      msgv= 'ATT. PDA attiv.'
184800080206     C                   ENDIF
184900080212     C                   ENDIF
185000080911     c* massa complessiva obbligatoria
185100080912     C                   if        v3cmcp = 0 and V3CPDD = ' '
185200080911     C                   MOVEL     MSG(24)       V1CMSG
185300080911     C                   SETON                                        901828
185400080911     C                   GOTO      ENDCON
185500080911     C                   ENDIF
185600160426     c* turno solo per parcel
185700160427     C                   if        v3ctpm = 'M' and v3ctur <> ' '
185800160503     c                             and *in27
185900160426     C                   MOVEL     MSG(36)       V1CMSG
186000160426     C                   SETON                                        902824
186100160426     C                   GOTO      ENDCON
186200160426     C                   ENDIF
186300160429     C                   if        v3ctpm = 'P' and v3ctur = ' '
186400160503     c                             and *in27
186500160429     C                   MOVEL     MSG(2)        V1CMSG
186600160429     C                   SETON                                        902824
186700160429     C                   GOTO      ENDCON
186800160429     C                   ENDIF
186900940809     C*
187000920921     C     ENDCON        ENDSR
187100030127     C**************************************************************************
187200030127     C*    SUBROUTINE 4° VIDEATA
187300030127     C**************************************************************************
187400030127     C     VIDEO4        BEGSR
187500030127     C*
187600030127     C                   SETOFF                                       111241
187700120308     c                   clear                   forzatura         1
187800120308     c                   setoff                                       88
187900030127     C*
188000030127     C                   MOVEL     V1CPDR        KPDR
188100030127     C     Kapd          CHAIN(N)  fiapd01L                           32
188200091106     c* verifico se abilitato alla manutenzione tariffe per poter inserire
188300091106     c* l'anagrafico o modi
188400091106     c                   exsr      sraut
188500091106     c   28              goto      for04
188600091106     c* inserimento
188700030127    1C     *IN32         IFEQ      '1'
188800030127     C                   SETON                                        11
188900030127     C                   EXSR      AZZ4
189000030127     C*
189100030127   X1C                   ELSE
189200030127     C                   SETON                                        12
189300030127     C     APDATB        IFNE      ' '
189400030127     C                   SETON                                        41
189500030127     C                   ENDIF
189600030127     C                   MOVEL     APDRSC        V3CRSC
189700030127     C                   MOVEL     APDRSF        V1CRSF
189800030127      *
189900030127     C                   MOVEL     APDKSC        V3CKSC
190000030127     C                   MOVEL     APDCSF        V3CCSF
190100120326     C                   MOVEL     APDKSC        saV3CKSC
190200120326     C                   MOVEL     APDCSF        saV3CCSF
190300120326      *
190400080911     c                   movel     apdflr        dapdflr
190500080911     c                   if        %subst(apdflr:9:1)= *blanks
190600080911     c                   clear                   v3cmcp
190700080911     c                   else
190800080911     c                   eval      v3cmcp= §apdmcp
190900080911     c                   end
191000030218      *
191100030218      *  Per le Coop il Nastro è oscurato e sempre a 0
191200030218     c                   if        v1ctip = 'A'
191300030128     C                   z-add     APDhpa        V3Chpa
191400030210     C                   z-add     APDhar        V3Char
191500030218     c                   else
191600030218     c                   z-add     0             v3chpa
191700030218     c                   z-add     0             v3char
191800030218     c                   end
191900030218      *
192000030127     C                   MOVEL     *BLANKS       V3DKSC
192100030127     C* DECODIFICO CODICE FORNITORE
192200030127    2C     V3CKSC        IFNE      0
192300030127     C     V3CCSF        ANDNE     *BLANKS
192400090330     c                   exsr      srrco
192500030127    1C                   END
192600030129    1C                   END
192700120522      **
192800120522     c* controllo se il fornitore è presente in archivio trazionisti
192900120522     c* e se sono un codice di A/D nel caso la società deve essere quella
193000120522     c* dei traini
193100120522ab   c                   move      'NO'          trazionista       2
193200120522ab   c                   clear                   COMETRAZ
193300120628      *
193400120522     c                   if        v1ctip = 'D'
193500120628      * società da anagr.
193600120628     c                   movel     apdcsf        cod_societa
193700120628     c                   exsr      TRULTRAZ_SUBR
193800120522      ***
193900120717     c                   if        esito <>' '
194000120608     c                             and ATSDFC > wdate
194100120522ab   c                   move      'SI'          trazionista       2
194200120522ab   c                   eval      COMETRAZ = ha_Contratto
194300120522     c                   endif
194400120628      *
194500120522     c                   endif
194600120522ab   C*-----
194700030127     C*
194800030127     C                   MOVEL     'S'           WRIEM
194900030127     C     FOR04         TAG
195000030127     C     WRIEM         IFEQ      'S'
195100030127     C                   WRITE     LV24T01
195200030127     C                   MOVEL     *BLANKS       WRIEM
195300030127     C                   ENDIF
195400030127     C                   EXFMT     LV24D04
195500030127     C*
195600030127     C                   SETOFF                                       285356
195700030211     C                   SETOFF                                       400203
195800140728     C                   SETOFF                                       541847
195900161108     C                   SETOFF                                       484645
196000120308      *
196100120308      *** Forza con il comando F11 il controllo di inesistenza
196200120308      *    dell'autista tra i contratti dei Trazionisti.
196300120308     c   88              if        *inKk
196400120308     c                   eval       forzatura = 'S'
196500120308     c                   setoff                                       88
196600120308     c                   end
196700120308     c
196800030127     C* CMD3 - FINE LAVORO
196900090330     C* CMD12 - RITORNO
197000090330     c                   if        *inkc or *inkl
197100090331     c                   if        flgcmt = *on
197200090331     c                   rolbk
197300090331     c                   end
197400090330     C                   GOTO      FINE4
197500090330     c                   end
197600091106     c* verifico se abilitato alla manutenzione tariffe per poter inserire
197700091106     c* l'anagrafico o modi
197800091106     c                   exsr      sraut
197900091106     c   28              goto      for04
197901170516     C* F02 Variazioni
197902170516if  1C                   IF        *inkb
197903170516     c                   movel     v1ctip        ilv37tip
197904170516     c                   movel     v1cpdr        ilv37pdr
197905170516     c                   movel     kpjbu         kpjbusav
197906170516     c                   movel     fnlv37ds      kpjbu
197907170516     c                   call      'FNLV37R'
197908170516     c                   parm                    kpjba
197909170516     c                   movel     kpjbusav      kpjbu
197910170516     c                   endif
198000030127     C*
198100030127     C* CMD16 - ANNULLAMENTO
198200030127     C   KQ              DO
198300030127     C*
198400030127     C* IMPOSSIBILE ANNULLARE la COOP  SE ESISTONO TARIFFE
198500030127     C     KPDR          SETLL     fiFGT01L
198600030127     C     KPDR          READE     fiFGT01L                               99
198700030127     C     *IN99         DOWEQ     *OFF
198800030127     C     FGTATB        IFEQ      ' '
198900030127     c     fgttsr        ifeq      'X'
199000030127     c     v1ctip        andeq     'C'
199100030127     C                   MOVEL     MSG(13)       V1CMSG
199200030127     C                   SETON                                        28
199300030127     C                   GOTO      FOR04
199400030127     C                   ENDIF
199500030127     C                   ENDIF
199600030127     C     KPDR          READE     fiFGT01L                               99
199700030127     C                   ENDDO
199800090610     C* IMPOSSIBILE ANNULLARE cod aff/defl se esitono tariffe non      da
199900090610     c* ancora ricevute firmate
200000080207     c     v1ctip        ifeq      'D'
200100090610     C     Kpdr          SETLL     fiadt01l
200200080207     C                   DO        *hival
200300090610     C     Kpdr          READE     fiadt01l
200400090610     c                   if        %eof(fiadt01l)
200500080207     c                   leave
200600080207     c                   end
200700090610     c                   if        adtatb <> ' '
200800090610     c                   iter
200900090610     c                   end
201000090610     c                   if        adtdrc = 0 or adtdst >= wdate
201100080207     C                   MOVEL     MSG(13)       V1CMSG
201200080207     C                   SETON                                        28
201300080207     C                   GOTO      FOR04
201400090610     c                   end
201500080207     C                   ENDDO
201600080207     c                   end
201700030127     C*
201800030127     C     Kapd          CHAIN     fiapd01L                           3298
201900030127     C     *IN98         IFEQ      *OFF
202000030127     C  N32              MOVE      'A'           APDATB
202100030127     C  N32              CLEAR                   APDDTR
202200030127     C  N32              CLEAR                   APDFTR
202300030127     C  N32              UPDATE    fiapd000                             98
202400170508      * preparazione dati richiamo PGM variazioni per ANNULLAMENTO
202500170508     c                   clear                   fnlv36ds
202600170508     c                   clear                   fiapdb
202700170510     c                   clear                   fiapda
202800170508     c                   clear                   fiapd4b
202900170510     c                   clear                   fiapd4A
202901170524     c                   eval      ILV36tip  = v1ctip
203000170508     c                   eval      ILV36PRU  = knmus
203100170508     c                   eval      ILV36NOJ  = knmeb
203200170508     c                   eval      ILV36PGM  = 'FNLV24R'
203300170508     c                   eval      ILV36CTA  = 'A'
203400170508     c                   eval      fiapdb =  fiapdds
203500170508      *richiama pgm registrazione variazioni
203600170508     c                   call      'FNLV36R'
203700170508     c                   parm                    fnlv36ds
203800170508     c                   parm                    fiapdb
203900170510     c                   parm                    fiapda
204000170508     c                   parm                    fiapd4b
204100170510     c                   parm                    fiapd4A
204300070208     c                   if        not *in98 and flgcmt = *on
204400070208     c                   commit
204500030127     C                   ENDIF
204600070208     C                   ENDIF
204601180116     C  N98              GOTO      FINE4
204700030127     C     *IN98         IFEQ      *ON
204800030127     C                   MOVEL     MSG(4)        V1CMSG
204900030127     C                   SETON                                        28
205000030127     C                   GOTO      FOR04
205100030127     C                   ENDIF
205200030127     C                   END
205300030127     C*
205400030127     C* CONTROLLI
205500030127     C                   EXSR      CONTRS4
205600030127      *
205700030127     C     *IN90         IFEQ      *ON
205800030127     C                   GOTO      FOR04
205900030127     C                   ELSE
206000030127     C     *INKE         IFEQ      *OFF
206100030127     C     *INKF         ANDEQ     *OFF
206200030127     C                   GOTO      FOR04
206300030127     C                   ENDIF
206400030127     C                   ENDIF
206500030127     C*
206600090331     c                   exsr      srcdf1
206700090331     c                   if        d27err <> ' '
206800090331     c                   goto      for04
206900090331     c                   end
207000090331     C                   EXSR      SCRIVI
207100070208     c                   if        not *in98 and flgcmt = *on
207200070208     c                   commit
207300070208     c                   end
207400030127     C* 98 ON - WRITE UPDATE O CHAIN NON ANDATE A BUON FINE
207500030127     C   98              GOTO      FOR04
207600030127     C*
207700030127     C     FINE4         ENDSR
207800090330     C**************************************************************************
207900090330     C*    CONTROLLo fornitore in proj
208000090330     C**************************************************************************
208100090330     C     srrco         BEGSR
208200120326      *
208300090330     c                   move      *all'0'       rcoksc
208400090330     c                   move      v3cksc        rcoksc
208500090330     c                   movel     v1cpdr        comuni            3
208600090330     C     Krco          CHAIN     anrco98j                           90
208700090330     c                   if        not *in90
208800090330     C                   MOVEL(p)  sogdes        v3dksc
208900090330     c                   if        rcounita <> comuni
209000090330     c                   seton                                        90
209100090330     c                   end
209200090422     c                   if        SOGCDFISC = ' ' or
209300090422     c                             SOGPARTIVA = ' '
209400090422     c                   seton                                        90
209500090422     c                   end
209600090506     c                   end
209700090506     c                   if        *in90
209800090330     C                   MOVEL     MSG(6)        V1CMSG
209900090330     C                   SETON                                        289056
210000090330    2C                   END
210100120326      *
210200090330    2C                   ENDsr
210300030127     C**************************************************************************
210400030127     C*    CONTROLLI 4° VIDEATA
210500030127     C**************************************************************************
210600030127     C     CONTRS4       BEGSR
210700030127     C*
210800030127     C                   SETOFF                                       90
210900030127     c                   do
211000090401     c* verifico se aut disaccreditato
211100090401     c*                  if        v1ctip = 'D'
211200090401     c*                  clear                   tradfi
211300090401     c*    ktra          chain     aitra05l
211400090401     c*                  if        %found(aitra05l) and tradfi<> 0
211500090401     c*                  eval      msgv='Autista disaccreditato'
211600090401     C*                  SETON                                        2890
211700090401     C*                  GOTO      ENDCON
211800090401    1C*                  END
211900090401    1C*                  END
212000090401     c*
212100030127     C                   CLEAR                   V3DKSC
212200030127     C*** DESCRIZIONE COOP  OBBLIGATORIA
212300030127    1C     V3CRSC        IFEQ      *BLANKS
212400030127     C                   MOVEL     MSG(3)        V1CMSG
212500030127     C                   SETON                                        285390
212600030127     C                   leave
212700030127    1C                   END
212800030127     C* descrizione per fatturaz.
212900030129     c                   if        v1crsf = *blanks
213000031204     c*                  movel(p)  v3crsc        v1crsf
213100031204     c                   seton                                        542890
213200031204     C                   MOVEL     MSG(3)        V1CMSG
213300031204     C                   leave
213400030129     c                   end
213500030127     C*
213600031121     C*  SOLO PER AFFLUENZE/DEFLUENZE
213700031121     C*  il codice fornitore deve appartenere ad un terminal
213800031121     C                   IF        v1ctip='D'
213900031121     c                   movel     v3cksc        cterm             3 0
214000031121     C                   reset                   FNLV55DS
214100031121     C                   eval      D55lin  = cterm
214200031121     C                   eval      D55drf  = wDATE
214300031121     C                   call      'FNLV55R'
214400031121     C                   parm                    FNLV55DS
214500151124     C                   IF        (d55tfp <> cterm  and
214600151124     C                             d55tfa <> cterm)  or
214700151124     C                             svterm <> cterm
214800151124     C                   reset                   FNLV55DS
214900151124     C                   eval      D55lin  = cterm
215000151124     c                   move      wdate         dataiso
215100151124     c     dataiso       adddur    30:*d         dataiso
215200151124     c                   move      dataiso       d55drf
215300151124     C                   call      'FNLV55R'
215400151124     C                   parm                    FNLV55DS
215500031121     C                   IF        (d55tfp <> cterm  and
215600031121     C                             d55tfa <> cterm)  or
215700031121     C                             svterm <> cterm
215800031121     C                   MOVEL     MSG(20)       V1CMSG
215900031121     C                   SETON                                        285690
216000031121     C                   leave
216100031121     c                   end
216200031121     c                   end
216300151124     c                   end
216400031121     C*
216500030127     C*** CODICE FORNITORE  E  SISTEMA INFORMATIVO
216600030127    1C     V3CKSC        IFeq      0
216700030127     C                   MOVEL     MSG(18)       V1CMSG
216800030127     C                   SETON                                        285690
216900030127     C                   leave
217000030127    1C                   ENDIF
217100030127    1C     V3CCSF        IFeq      *BLANKS
217200030127     C                   MOVEL     MSG(19)       V1CMSG
217300030127     C                   SETON                                        284090
217400030127     C                   leave
217500030127    1C                   ENDIF
217600030127      *
217700030127    1C     V3CKSC        IFNE      0
217800030127     C     V3CCSF        ANDEQ     *BLANKS
217900030127     C                   MOVEL     MSG(8)        V1CMSG
218000030127     C                   SETON                                        284090
218100030127     C                   leave
218200030127    1C                   ENDIF
218300030127     C*
218400030127    1C     V3CKSC        IFEQ      0
218500030127     C     V3CCSF        ANDNE     *BLANKS
218600030127     C                   MOVEL     MSG(7)        V1CMSG
218700030127     C                   SETON                                        285690
218800030127     C                   leave
218900030127    1C                   ENDIF
219000080107     c* controllo società
219100080107     C     V3CCSF        ifne      *BLANKS
219200080107     C     v3ccsf        CHAIN     ansif01l
219300080107     c                   if        not %found(ansif01l) or
219400080107     c                             (%found(ansif01l) and SIFDTFIVL <>
219500080107     c                             *loval)
219600080107     c                   seton                                        289040
219700080107     C                   MOVEL     MSG(21)       V1CMSG
219800080107     c                   end
219900080107     c                   end
220000120516      *
220100030211     C*** CONTROLLO SE PRESENTE IN anrco98j
220200030127     C     V3CKSC        IFNE      0
220300090330     c                   exsr      srrco
220400030127     C   90              leave
220500030127     C                   ENDIF
220600120308      **
220700120308     c* controllo se il fornitore è presente in archivio trazionisti
220800120308     c* e se sono un codice di A/D nel caso la società deve essere quella
220900120308     c* dei traini
221000120516ab   c                   move      'NO'          trazionista       2
221100120517ab   c                   clear                   COMETRAZ
221200120308     c                   if        v1ctip = 'D'
221300120522      ***
221400120628      * società da video
221500120628     c                   movel     v3cCSF        cod_societa
221600120628     c                   exsr      TRULTRAZ_SUBR
221700120522      ***
221800120717     c                   if        esito <>' '
221900120608     c                             and ATSDFC > wdate
222000120516ab   c                   move      'SI'          trazionista       2
222100120517ab   c                   eval      COMETRAZ = ha_Contratto
222200120308      *
222300120308      *   possibilità di forzare la società per un autista che pur essendo
222400120308      *   trazionista NON esegue i TRAINI STANDARD ....quindi deve poter
222500120308      *   immettere una società diversa da quella dei TRAINI.
222600120308      *
222700120308     c                   if        forzatura = *blank
222800120308     c                   if        v3ccsf <> societa
222900120308     C                   MOVEL     MSG(26)       V1CMSG
223000120308     C                   SETON                                        284090
223100120308     c                   seton                                        88
223200120308     C                   leave
223300120308     c                   endif
223400120308     c                   end
223500120308      *
223600120308     c                   endif
223700120308     c                   endif
223800120516ab   C*-----
223900120516ab   C*  deve controllare per affluenze/defluenze
224000120516ab   C*   che la società sia quella operativa della filiale
224100120516ab   C                   exsr      CHK_SOC_OPE
224200120517ab   C*----- x Affl./Defl.
224300120517     c                   if        v1ctip = 'D'
224400120516ab   c                   if        v3cCSF <> idsocieta AND trazionista = 'NO'
224500120627ab   c                             and v3cCSF = societa
224600120627ab   c                   seton                                        289040
224700120627ab   C                   MOVEL     MSG(29)       V1CMSG
224800120627ab   c                   leave
224900120627      ***
225000120627ab   c                   elseIF    (v3cCSF <> idsocieta and v3cCSF <> societa)
225100120516ab   c                   seton                                        289040
225200120516ab   C                   MOVEL     MSG(21)       V1CMSG
225300120516ab   c                   leave
225400120516ab   C                   end
225500120517ab   C                   end
225600120517ab   C*----- x COOP
225700120830     c                   if        v1ctip = 'C'
225800120830     c                   if        v3cCSF <> idsocieta
225900120517ab   c                   seton                                        289040
226000120517ab   C                   MOVEL     MSG(21)       V1CMSG
226100120517ab   c                   leave
226200120829     c                   else
226300120830     c* solo se cambia la società operativa
226400120830     c                   if        valDatIniz <> *loval and
226500120830     C                               V3Ccsf <> apdcsf and apdcsf <> ' '
226600120829     c                   move      valDatIniz    valdat            8 0
226700120829     c                   clear                   esito_ctt         1
226800120829     c* se la società è corretta verifico che non ci siano conteggi più
226900120829     c* vecchi dell'inzio della soc. non ancora confermati
227000120829     C/EXEC SQL
227100120829     C+ SELECT '1' into :esito_ctt
227200120829     C+    FROM fictt00f WHERE cttpdr = :V1CPDR and
227300120830     C+     cttddc < :valdat and cttfvl <> 'C' and cttsoc = :apdcsf
227400120829     C+ FETCH first row only
227500120829     C/END-EXEC
227600120829      *   se trovato non deve permettere la variazione
227700120829     c                   if        SQLcod = 0 and esito_ctt ='1'
227800120829     C                   MOVEL     MSG(30)       V1CMSG
227900120829     C                   SETON                                        28
228000120829     C                   leavesr
228100120829     c                   end
228200120829     c                   end
228300120830     c                   end
228400120517ab   C                   end
228500120517ab   C*-----
228600120308     c
228700071203     c* controllo il fornitore di fatturazione
228800071203     c                   exsr      srcdf
228900071203     C   90              leave
229000080911     c* controllo massa complessiva solo per aff/defl
229100080911    1C                   IF        v3cmcp = 0 and v1ctip = 'D'
229200080911     C                   MOVEL     MSG(24)       V1CMSG
229300080911     C                   SETON                                        281890
229400080911     C                   leave
229500080911    1C                   ENDIF
229600030127     c                   enddo
229700030127     c*
229800030127     C                   ENDSR
229900940809     C**************************************************************************
230000090330     C     srcdf         BEGSR
230100940809     C**************************************************************************
230200940809     C*
230300080102     C     V3CKSC        IFNE      0
230400120326      *
230500090330     C*  controlla e aggiorna il fornitore di fatturazione
230600071203     c                   clear                   fnlv26ds
230700071203     c                   eval      d26soc = v3ccsf
230800071203     c                   movel     *all'0'       d26ksc
230900071203     c                   move      v3cksc        d26ksc
231000071203     c                   movel(p)  fnlv26ds      kpjbu
231100071203     c                   call      'FNLV26R'
231200071203     c                   parm                    kpjba
231300071203     c                   movel     kpjbu         fnlv26ds
231400071203     c                   if        d26err <> ' '
231500071203     C                   MOVEL     MSG(9)        V1CMSG
231600071203     c                   seton                                        564090
231700071203     c                   seton                                        28
231800080102     c                   end
231900090331     c                   end
232000071203     c*
232100071203     C                   ENDSR
232200090331     C**************************************************************************
232300090331     C     srcdf1        BEGSR
232400090331     C**************************************************************************
232500090331     C*
232600090331     C     V3CKSC        IFNE      0
232700120326      *
232800120326      *  Da eseguire solo se cambiato il codice Fornitore o la società
232900120326     C                   if          V3Cksc <> saV3Cksc or
233000120326     C                               V3Ccsf <> saV3Ccsf
233100120326     C                   eval        saV3Cksc = V3Cksc
233200120326     C                   eval        saV3Ccsf = V3Ccsf
233300120326      *
233400090331     C*  controlla e aggiorna le tariffe aut/aff/defl
233500090331     c                   if        v1ctip = 'A' or v1ctip = 'D'
233600090331     c                   clear                   fnlv27ds
233700090331     c                   eval      d27cmt = flgcmt
233800090331     c                   eval      d27tip = v1ctip
233900090331     c                   movel     v1cpdr        d27pdr
234000090331     c                   eval      d27soc = v3ccsf
234100090331     c                   movel     *all'0'       d27ksc
234200090331     c                   move      v3cksc        d27ksc
234300090331     c                   movel(p)  fnlv27ds      kpjbu
234400090331     c                   call      'FNLV27R'
234500090331     c                   parm                    kpjba
234600090331     c                   movel     kpjbu         fnlv27ds
234700090331     c                   if        d27err =  '1'
234800090331     C                   MOVEL     MSG(5)        V1CMSG
234900090428     C                   MOVEL     'S'           WRIEM
235000090331     c                   seton                                        564090
235100090331     c                   seton                                        2898
235200090331     c                   end
235300090331     c                   end
235400120326     c                   endIF
235500090331     c                   end
235600090331     c*
235700090331     C                   ENDSR
235800071203     C**************************************************************************
235900071203     C*    SR IMMISSIONE/AGGIORNAMENTO/RIATTIVAZIONE
236000071203     C**************************************************************************
236100071203     C     SCRIVI        BEGSR
236200071203     C*
236300080206     c                   setoff                                       28
236400170113     c                   eval      si = '0'
236500170113     C*
236600170113     C* Solo se non è chiamato come COOP  (quindi x città o affl.defl.)
236700160921     c                   if        v1ctip <>'C'
236800170113     C*
236900160921     c                   if        v1ctip = 'A'
237000160921     c                   move      'D'           citaff
237100160921     c                   else
237200160921     c                   move      'A'           citaff
237300160921     c                   endif
237400160921     C/EXEC SQL
237500160921     C+  SELECT '1' into :si
237600160921     C+   FROM fiapd00f WHERE apdatb = ' ' and apdpdr =:v1cpdr and
237700160921     C+   apdksc = :v3cksc and apdcsf = :v3ccsf and apdtip= :citaff
237800160921     C+  FETCH first row only
237900160921     C/END-EXEC
238000160921     c                   endif
238100160921      *
238200940809     C                   MOVEL     V1CPDR        KPDR
238300021129     C     KAPD          CHAIN     fiapd01L                           3298
238400931130     C*
238500170113     C* Solo in Immissione e NON in manutenzione
238600170113     C*     NON DEVE PERMETTERE di inserire
238700170113     c   32              if        si = '1'
238800170113     c                   seton                                        98
238900170113     c                   goto      salta
239000170113     C*                ===================
239100170113     c                   endif
239200170113     C*
239300931130     C     *IN98         IFEQ      *OFF
239400170508      *registra variazioni prepara dati generali e BEFORE
239500170508     c                   clear                   fnlv36ds
239600170510     c                   clear                   fiapdb
239700170510     c                   clear                   fiapda
239800170508     c                   clear                   fiapd4b
239900170510     c                   clear                   fiapd4A
239901170524     c                   eval      ILV36tip  = v1ctip
240000170508     c                   eval      ILV36PRU  = knmus
240100170508     c                   eval      ILV36NOJ  = knmeb
240200170508     c                   eval      ILV36PGM  = 'FNLV24R'
240300170508     c                   if        *in32
240400170508     c                   eval      ILV36CTA  = 'I'
240500170508     c                   else
240501170516     c   ke              eval      ILV36CTA  = 'R'
240600170516     c  nke              eval      ILV36CTA  = 'M'
240700170508     c                   eval      fiapdB  = fiapdds
240800170508     c                   endif
240900170508      * fine check variazioni
241000000726     C                   MOVE      ' '           APDATB
241100021129     C                   MOVEL     V1CTIP        APDTIP
241200021129     C                   MOVEL     V1CPDR        APDPDR
241300940809     C                   MOVEL     V3CRSC        APDRSC
241400021129     C                   MOVEL     V1CRSF        APDRSF
241500120626     C                   clear                   APDZNP
241600940809     C                   MOVEL     V3CKSC        APDKSC
241700940809     C                   MOVEL     V3CCSF        APDCSF
241800940809     C                   MOVEL     V3CPKL        APDPKL
241900940809     C                   MOVEL     V3CVLU        APDVLU
242000940809     C                   MOVEL     V3CPDD        APDPDD
242100940809     C                   MOVEL     V3CFGR        APDFGR
242200161025     C                   MOVEL     V3CSNU        APDSNU
242300941024     C                   MOVEL     V3CFCM        APDFCM
242400940809     C                   MOVEL     V3CETM        APDETM
242500940809     C                   MOVEL     V3CETP        APDETP
242600940809     C                   MOVEL     V3CPKM        APDPKM
242700940809     C                   MOVEL     V3CPKP        APDPKP
242800021129     C                   MOVEL     V1CCAM        APDCAM
242900021129     C                   MOVEL     V1CCAP        APDCAP
243000021129     C                   MOVEL     V1CSCM        APDSCM
243100021129     C                   MOVEL     V1CSCP        APDSCP
243200030128     C                   z-add     v3chpa        APDHPA
243300030128     C                   z-add     v3char        APDHAR
243400050518     c                   movel     v3cgge        §apdgcd
243500050804     c                   movel     v3ckm         §apdkm
243600050519     c                   movel     v3ctpm        §apdtpm
243700160426     c                   movel     v3ctur        §apdturno
243800110303     c                   eval      apdptp  = v3cbans
243900050523     c                   if        v3ctpm = 'P'
244000050523     c                   clear                   §apdtpm
244100050523     c                   end
244200160426     c                   if        v3ctur = '1'
244300160426     c                   clear                   §apdturno
244400160426     c                   end
244500070723     c                   movel     v3cpda        §apdpda
244600080206     c                   movel     v3ctstpda     §apdtstpda
244700080911     c                   eval      §apdmcp = v3cmcp
244800090706     c                   eval      §apdmezzo = v3cmezzo
244900161025     c                   eval      §apdspec = v3cspec
245000161003     c                   eval      §apdats = v3cats
245100161108     c                   eval      §apdaat = v3caat
245200050519     c                   movel(p)  dapdflr       apdflr
245300920921     C*
245400060322     C                   z-add     wdate         APDDTR
245500021129     C                   CLEAR                   APDFTR
245600021129     C*
245700021129     C   32              WRITE     fiapd000                             98
245800021129     C  N32              UPDATE    fiapd000                             98
245900130726     c                   movel     kpdr          kpdr3             3 0
246000130726     c     kapd4         chain     fiapd41l
246100130726     c                   if        %found(fiapd41l)
246200170508      *valorizza ds per vaiazioni
246300170508     c                   eval      fiapd4B  = fiapd4ds
246400170508      *
246500130731     c                   clear                   apd4atb
246600130729     c                   movel     v3ctel        §APD4TEL
246700130729     c                   movel     dapd4tel      apd4uni
246800130729     c                   update    fiapd400
246900130729     c                   else
247000130729     c                   clear                   apd4atb
247100130729     c                   move      v1ctip        apd4tipa
247200130729     c                   movel     'TEL'         apd4trd
247300130729     c                   z-add     kpdr          apd4pdr
247400130729     c                   movel     v3ctel        §APD4TEL
247500130729     c                   movel     dapd4tel      apd4uni
247600130729     c                   write     fiapd400
247700130726     c                   endif
248300170508      *ripristino e modifica
248900170508     c                   eval      fiapdA  = fiapdds
249000170508     c                   eval      fiapd4A  = fiapd4ds
249200170508      *richiama pgm registrazione variazioni
249300170508     c                   call      'FNLV36R'
249400170508     c                   parm                    fnlv36ds
249500170508     c                   parm                    fiapdb
249600170510     c                   parm                    fiapda
249700170508     c                   parm                    fiapd4b
249800170510     c                   parm                    fiapd4A
249900931130     C                   ENDIF
250000940809     C*
250100160921     c     salta         tag
250200160921      *
250300940809     C     *IN98         IFEQ      *ON
250400160921     c                   if        si = '1'
250500160921     C                   MOVEL     MSG(37)       V1CMSG
250600160921     c                   else
250700940809     C                   MOVEL     MSG(4)        V1CMSG
250800160921     c                   endif
250900940809     C                   SETON                                        28  53
251000940809     C                   ENDIF
251100920921     C*
251200920921     C                   ENDSR
251300120216     C**************************************************************************
251400120216     C*    CONTROLLo per a/d se il fornitore è un trazionista certificato
251500120216     C**************************************************************************
251600120216     C     veriftraz     BEGSR
251700120628     C*
251800120217     c                   clear                   vtraz
251900120217     c                   clear                   vsckscd
252000120216     c                   move      *all'0'       rcoksc
252100120216     c                   move      vscksc        rcoksc
252200120216     C     Krco2         CHAIN     anrco98j                           90
252300120216     c                   if        not *in90
252400120217     c                   eval      vsckscd = sogdes
252500120628      * società da anagr.
252600120628     c                   movel     apdcsf        cod_societa
252700120628     c                   exsr      TRULTRAZ_SUBR
252800120717     c                   if        esito <>' '
252900120608     c                             and ATSDFC > wdate
253000120216     c                   eval      vtraz = 'T'
253100120216     C                   SETON                                        25
253200120307     c                   endif
253300120216     c                   endif
253400120628     C*
253500120216     c                   endsr
253600120217     C**************************************************************************
253700120217     C*    stampa elenco autisti A/D tutti o solo trazionisti
253800120217     C**************************************************************************
253900120628     C     TRULTRAZ_SUBR begsr
254000120628      *
254100120628     c                   clear                   partraz
254200120628     c                   clear                   dsAIATS
254300120628     c                   z-add     cod_societa   parsoc
254400120628     c                   movel     sogpartiva    pariva
254500120628     c                   call      'TRULTRAZ'
254600120628     c                   parm                    partraz
254700120628     c                   parm                    dsAIATS
254800120628      *
254900120628     c                   endsr
255000161003     C**************************************************************************
255100161003     C*    verifica o ricerca tabella ATS
255200161003     C**************************************************************************
255300161003     C     checkATS      begsr
255400161003      * Aggancio tabella "ATS" per controllo o ricerca
255500161003     C                   CLEAR                   tibs02ds
255600161003     c                   if        v3cats = '?'
255700161003     C                   MOVEL     'R'           T02MOD
255800161003     C                   MOVEL     KNSIF         T02SIF
255900161003     C                   MOVEL     'ATS'         T02COD
256000161003     c                   else
256100161003     C                   MOVEL     'C'           T02MOD
256200161003     C                   MOVEL     KNSIF         T02SIF
256300161003     C                   MOVEL     'ATS'         T02COD
256400161003     C                   movel     v3cats        T02KE1
256500161003     c                   endif
256600161003      *
256700161003     C                   CALL      'TIBS02R'
256800161003     C                   PARM                    KPJBA
256900161003     C                   PARM                    tibs02ds
257000161003     C     T02ERR        IFEQ      *BLANKS
257100161003     C                   MOVEL     T02UNI        dats
257200161027     c                   movel     d§atsdes      v3catsd
257300161003     c                   movel     t02ke1        v3cats
257400161003     C                   ELSE
257500161003     c                   clear                   v3catsd
257600161003     c                   clear                   v3cats
257700161003     C                   CLEAR                   Dats
257800161003     C                   ENDIF
257900161003      *
258000161003     c                   endsr
258100161108     C**************************************************************************
258200161108     C*    verifica o ricerca tabella AAT
258300161108     C**************************************************************************
258400161108     C     checkAAT      begsr
258500161108      * Aggancio tabella "AAT" per controllo o ricerca
258600161108     C                   CLEAR                   tibs02ds
258700161108     c                   if        v3caat = '?'
258800161108     C                   MOVEL     'R'           T02MOD
258900161108     C                   MOVEL     KNSIF         T02SIF
259000161108     C                   MOVEL     'AAT'         T02COD
259100161108     c                   else
259200161108     C                   MOVEL     'C'           T02MOD
259300161108     C                   MOVEL     KNSIF         T02SIF
259400161108     C                   MOVEL     'AAT'         T02COD
259500161108     C                   movel     v3caat        T02KE1
259600161108     c                   endif
259700161108      *
259800161108     C                   CALL      'TIBS02R'
259900161108     C                   PARM                    KPJBA
260000161108     C                   PARM                    tibs02ds
260100161108     C     T02ERR        IFEQ      *BLANKS
260200161108     C                   MOVEL     T02UNI        daat
260300161108     c                   movel     d§aatdes      v3daat
260400161108     c                   movel     t02ke1        v3caat
260500161108     C                   ELSE
260600161108     c                   clear                   v3daat
260700161108     c                   clear                   v3caat
260800161108     C                   CLEAR                   Daat
260900161108     C                   ENDIF
261000161108      *
261100161108     c                   endsr
261200120628     C**************************************************************************
261300120628     C*    stampa elenco autisti A/D tutti o solo trazionisti
261400120628     C**************************************************************************
261500120628     C     stampa        begsr
261600120217     c                   do        *hival        leggi             4 0
261700120217     c     leggi         chain     lv24s02                            90
261800120217     c                   if        *in90
261900120217     c                   leave
262000120217     c                   endif
262100120217     c                   if        *inkh and vtraz = *blank
262200120217     c                   iter
262300120217     c                   endif
262400120517     c                   setoff                                       85
262500120517     c                   if        v1ctip = 'A'
262600120517     c                   seton                                        85
262700120517     c                   end
262800120220     c                   if        line > 65
262900120217     c                   except    testa
263000120217     c                   endif
263100120217     c                   except    riga
263200120217     c                   enddo
263300120217     c                   setoff                                       90
263400120217     c                   endsr
263500940809     C**************************************************************************
263600940809     C*     R O U T I N E     I N I Z I A L E
263700940809     C**************************************************************************
263800940809     C     *INZSR        BEGSR
263900940809     C*
264000940809     C     *LIKE         DEFINE    APDPDR        KPDR
264100950314RM*  C     *LIKE         DEFINE    V1FPAD        CDU
264200940809     C                   MOVEL     *BLANKS       WRIEM             1
264300940909     C*
264400080207     C     Katt          KLIST
264500080207     C                   KFLD                    v3ccsf
264600080207     C                   KFLD                    rcoksc
264700161003     C     Ktbe          KLIST
264800161003     C                   KFLD                    tbecod
264900161003     C                   KFLD                    tbeke1
265000090401     C     Ktra          KLIST
265100090401     C                   KFLD                    kpdr
265200090401     C                   KFLD                    tradfi
265300080207     C     Kapd          KLIST
265400080207     C                   KFLD                    v1ctip
265500080207     C                   KFLD                    kpdr
265600130726     C     Kapd4         KLIST
265700130726     C                   KFLD                    v1ctip
265800130726     C                   KFLD                    kpdr
265900130726     C                   KFLD                    ktrd
266000130726     c                   move      'TEL'         ktrd              3
266100070208     C     Kapd21        KLIST
266200070208     C                   KFLD                    v1ctip
266300070208     C                   KFLD                    kpdr
266400070208     C     Kapd2         KLIST
266500070208     C                   KFLD                    v1ctip
266600070208     C                   KFLD                    kpdr
266700070208     C                   KFLD                    apd2fpp
266800030129     C*
266900030129     C     Krco          KLIST
267000030129     C                   KFLD                    v3ccsf
267100030129     C                   KFLD                    rcosnatura
267200030129     C                   KFLD                    rcoksc
267300030129     c                   eval      rcosnatura = 'F'
267400120216     C     Krco2         KLIST
267500120216     C                   KFLD                    apdcsf
267600120216     C                   KFLD                    rcosnatura
267700120216     C                   KFLD                    rcoksc
267800021129     C*
267900120216     C     Ktab          KLIST
268000120216     C                   KFLD                    KUTET
268100120216     C                   KFLD                    tblcod
268200120216     C                   KFLD                    tblkey
268300120626     C                   Z-ADD     1             KUTET             1 0
268400031121      *
268500031121      * Data odierna in formato AAAAMMGG
268600031121     C                   clear                   WLBDAT
268700031121     C                   move      *date         G02dat
268800031121     C                   call      'XSRDA8'
268900031121     C                   parm                    WLBDAT
269000031121     C                   move      G02inv        wDATE
269100070108     C                   move      G02inv        wDATEc            8
269200031121      *
269300120518ab   C*     Inizializza
269400120518ab    /FREE
269500120518ab     TibsSof_Init();
269600120518ab    /END-FREE
269700120516     C*
269800120516     C                   ENDSR
269900141106      /FREE
270000141106       //--------------------------------------------------------------
270100141106       //?Richiamo Service Program UBCHKCEL                             ?
270200141106       //--------------------------------------------------------------
270300141106       BEGSR ChkTeL;
270400141106
270500141106         pInCell  = V3ctel;
270600141106         pOutCell = *blank;
270700141106         pOutErr = *blank;
270800141106
270900141106         if V1InCellOp = *blank;
271000141106           // è il NOPASS
271100141106           EsitoCel = UBCHKCEL_Check ( pInCell : pOutCell :
271200141106                                      pOutErr );
271300141106         else;
271400141106           pInCellOpz = V1InCellOp;
271500141106           EsitoCel = UBCHKCEL_Check ( pInCell : pOutCell :
271600141106                                      pOutErr: pInCellOpz );
271700141106         endif;
271800141106
271900141106         V3ctel = pOutCell;
272000141106         V1Err     = pOutErr ;
272100141106
272200141106       ENDSR;
272300141106      /END-FREE
272400120518ab   C**************************************************************************
272500120518ab    * Controlla la Società operativa della Filiale e quella dei trazionisti
272600120518ab   C**************************************************************************
272700120516ab   C     CHK_SOC_OPE   begsr
272800120518ab   **
272900120518ab   ** 1°test per verificare la società
273000120518ab   **      occorre impostare 2 parametri prima di chiamare la routine:
273100120518ab   **           (1)      Filiale (già impostata fra i parametri)
273200120518ab   **           (2)      Data Validità
273300120518ab   **  Data del giorno
273400120518ab   c                   MOVE      *DATE         dtVALIDITA
273500120518ab   c                   exsr      TEST_SOCieta
273600120518ab   **
273700120518ab   ** se non coincide con la società a video, prima di dare l'errore,
273800120518ab   ** controlla se ce n'è un'altra in un periodo successivo.
273900120518ab   **      ...Questo per permettere di inserire un'anagrafica per
274000120518ab   **       il prossimo futuro.
274100120518ab    **
274200120518ab    ** Se la data NON coincide con quella impostata a video, occorre
274300120518ab    **  provare a verificare se esiste in un periodo successivo.
274400160729ab    **  per coop solo se sono in inserimento di un codice
274500160729ab   c                   if        v3cCSF <> idsocieta or
274600160801     c                             (v3cCSF <> idsocieta and
274700160801     c                             v1ctip = 'C' and apdcsf = ' ')
274800120518ab    **
274900120518ab   c                   if        idsocieta = *blank
275000120518ab   c                               or
275100120518ab   c                             VALdatFINE <> *HIVAL
275200120518ab   **
275300120518ab   **  deve impostare la data di riferimento per controllare se
275400120518ab   **   c'è la società su un altro periodo successivo.
275500120518ab   c                   if        idsocieta = *blank
275600120518ab   c                   eval      dtVALIDITA = *hival
275700120518ab   C                   else
275800120518ab   c     VALdatFINE    adddur    1:*D          dtVALIDITA
275900120518ab   C                   end
276000120518ab   **
276100120518ab   c                   exsr      TEST_SOCieta
276200120518ab   **
276300120517ab   C                   end
276400120518ab   C                   end
276500120518ab   **
276600120516ab   C                   ENDSR
276700120518ab   C**************************************************************************
276800120517ab    * Controlla se nel periodo successivo è prevista un'altra società
276900120518ab   C**************************************************************************
277000120518ab   C     TEST_SOCieta  begSR
277100120518ab    *
277200120518ab    /FREE
277300120518ab
277400120518EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( V1FPAD
277500120518EDPDC                                             : dtVALIDITA
277600120518EDPDC                                             : 'O'
277700120518EDPDC                                             : valDatIniz
277800120518EDPDC                                             : valDatFine
277900120518ab                                                );
278000120518ab
278100120518EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
278200120518EDPDC    // Gestire l'errore.
278300120518ab     ENDIF;
278400120518ab
278500120518ab    /END-FREE
278600120517ab   C                   ENDSR
278700120517     C**************************************************************************
278800120515     Oqsysprt   e            TESTA          1  2
278900120217     o                                            8 'FNLV24R'
279000120217     o                       knsif             +  2
279100120217     o                       knmus             +  2
279200120220     O                                           50 'Stampa Elenco'
279300120220     o                       tipoelenco        +  1
279400120217     O                                         +  1 'della filiale '
279500120217     o                       v1fpad            +  1
279600120220     o                       orgdes            +  1
279700120217     o                       *date         y   +  3
279800120217     o                                         +  2 'Pag.'
279900120217     o                       page              +  1
280000120217     O          e            TESTA       1  1
280100120515     O                                            9 'Autista'
280200120515     o                                           14 'Traz'
280300120220     O                                           31 'Nome autista'
280400120217     O                                           50 'Società'
280500120217     O                                           60 'Fornitore'
280600120517     O               85                         113 'PDA'
280700120217     O          e            riga        1
280800120217     o                       vscpdr               9
280900120217     o                       vtraz               13
281000120220     o                       vscrsc            +  6
281100120217     o                       vsocope           +  5
281200120220     o                       vscksc            +  2
281300120217     o                       vsckscd           +  1
281400120522     o               85      vscPDAsoc          113
281500120217      *------------------------------------------------------------------------
281600090113** cmd1
281700090113OVRDBF FILE(ansif01l) TOFILE(pjbargru/ansif01l)
281800090113** cmd2
281900090113OVRDBF FILE(ansif01l) TOFILE(gaitrased/ansif01l)
282000940809**
282100021129Campo obbligatorio: immettere un codice o '?' per avere l'elenco               1
282200021129Immettere un codice valido                                                     2
282300021129Immettere la descrizione                                                       3
282400000726Errori verificatisi nell'aggiornamento: RIPROVARE O AVVISARE IL CED (SEDE)     4
282500090330Errore nelle tariffe o nei contratti                                           5
282600090506Codice fornitore inesistente o errato                                          6
282700940809Se immesso codice sistema informativo, immettere anche il codice fornitore     7
282800940809Se immesso codice fornitore, immettere anche codice sistema informativo        8
282900071203Errore fornitore di fatturazione: avvisare uff. IVA                            9
283000071205Non è possibile eseguire l'annullamento. Codice autotrasportatore "tecnico"   10
283100060203Zona NON abilitata                                                    TAB-3D  11
283200940909Codice filiale errata o non gestita                                           12
283300021129Non è possibile eseguire l'annullamento perchè utilizzato in alcune tariffe   13
283400021129Immettere filiale valida o '?' nel codice l'elenco                            14
283500010206Zona errata                                                           TAB-3D  15
283600010206Zona gia' inserita                                                            16
283700010206Non è possibile scegliere una zona relativa ad altro P.O.             TAB-3D  17
283800020328Codice Fornitore Obbligatorio                                                 18
283900020328Sistema informativo Obbligatorio                                              19
284000031121Codice Fornitore non Ammesso (Deve Appartenere al Terminal)                   20
284100080107Codice Società non utilizzabile                                               21
284200080206Filiale non ancora attiva per gestione PDA                                    22
284300080206Scelta incongruente                                                           23
284400100421Massa complessiva obbligatoria                                                24
284500100421Utente non abilitato all'inserimento dell'anagrafico                          25
284600120308Se Fornitore fa TRAINI STANDARD,la società è errata.F11 Forza diversa SOC.    26
284700120308Fornitore presente come trazionista la società non corrisponde alla TRAINI    27
284800120515Non è possibile annullare l'Autista in quanto ha ancora distinte APERTE.      28
284900120627Cod.Società errato non essendo il fornitore un TRAZIONISTA                    29
285000120829Esistono conteggi non ancora confermati !!!                                   30
285100160405Il range di codici che va dal 9500 al 9509 è riservata al CED                 31
285200140728Autista escluso da fatturazione etich/pick/carico/scarico devono essere = N   32
285300140728Autista in autofatturazione "flag Merce portata a magazzino"deve essere = " " 33
285400140728Autista escluso da autofatturazione "Merce portata a magazzino"deve essere = S34
285500161108Il numero di telefono inserito non è formalmente corretto                     35
285600160428Il turno è ammesso solo se tipo distinta è Parcel                             36
285700160921Errore esiste un codice autista in città/affl. con le stesse caratteristiche  37
285800161110Codice Territorio errato o non presente in tabella                            38
285900161110Codice Tipo Attività errato o non presente in tabella                         39
286000171102Autista escluso da fatturazione il codice fornitore NON deve essere impostato 40
