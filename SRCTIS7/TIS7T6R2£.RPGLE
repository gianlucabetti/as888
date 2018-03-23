000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400170512     Ftntbe01l  if   E           K DISK
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020916     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500010330     D esito           s              1
001600010330     D prmlit          s             10
001700010330     D prmfir          s             10
001800010330     D wrkesito        s                   like(esito)
001900010330     D rrnum           s              6  0 INZ(*zeros)
002000010612     D*
002100010612     D fnvataar        ds                  inz
002200010612     D  a_vatatb               1      1
002300010612     D  a_vatccm               3      9
002400010612     D  a_vatlnp              11     13
002500010612     D  a_vataas              15     18
002600010612     D  a_vatnrs              20     21
002700010612     D  a_vatnsp              23     29
002800010612     D  a_vattrc              30     30
002900010612     D  a_vatnot              31     65
003000100301
003100100301     D*------------------
003200100301     D* DS FIVAT DA EASYWEB
003300100301     D*------------------
003400100301     D ds_easyweb      ds
003500100301     D   ds_vat                            like(fnvataar)
003600100301     D   a_filler1                    1
003700100301     D   a_vatfgs                          like(a_vatlnp)
003800170512
003900170512     D DVPC          e ds                  inz
004000170512     D skVPC999        s              7  0 inz dim(200)
004100170512     D jVPC            s              3  0 inz
004200010330
004300010330
004400010330
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800010601     C*
004900010601     C                   exsr      opeini
005000010612     C                   exsr      rwvat
005100010601     C*
005200010601     C                   seton                                        lr
005300010330
005400010601
005500010601
005600010330     C*--------------------------------------------------------
005700020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
005800010330     C*--------------------------------------------------------
005900010612     C     RWVAT         BEGSR
006000010330     C*
006100010330     C                   if        not %open(tivin00r)
006200010330     C                   open      tivin00r
006300010330     C                   endif
006400020916     C                   if        not %open(fivatwwr)
006500020916     C                   open      fivatwwr
006600010330     C                   endif
006700010330     C*
006800010604     C                   clear                   §CTROK
006900010604     C                   clear                   §CTRMO
007000010604     C                   clear                   §CTRNO
007100010330     C*
007200010330     C                   DO        *HIVAL
007300010330     C*
007400010330     C                   READ      tivin00r                               70
007500010618     C*
007600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
007700010618     C                   if        vinflg = '1'
007800010618     C                   eval      flgOk = '1'
007900010618     C                   endif
008000010618     C*
008100010330     C                   if        vindta > *blanks
008200010330     C                   add       1             rrnum
008300010330     C*
008400010601     C                   if        *in70 = *off and
008500010330     C                             (vinflg = *blanks
008600010330     C                              or vinflg = '0'
008700010330     C                              or vinflg = '2')
008800010711     C*
008900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
009000010711     C                   if        vinflg = *blanks or vinflg = '0'
009100010711     C                   clear                   vinmsg
009200010711     C                   endif
009300010711     C*
009400100301     C                   eval      ds_easyweb = vindta
009500100301     C                   eval      fnvataar   = ds_vat
009600010601     C*
009700010330     C                   exsr      inzvar
009800010330     C                   exsr      defcam
009900010612     C                   exsr      impvat
010000170504     C*
010100170504     C* Effettuo considerazioni x elaborazioni "multi-filiale"
010200170504     C                   if        vlrpoi = 999
010300170512     C                   exsr      chkVPC999
010400170512     C                   if        VATFGS = 999 or *IN55
010500170504     C                   movel     VATCCM        VATFGS
010600170504     C                   endif
010700170504     C                   eval      invfil = VATFGS
010800170504     C                   else
010900170504     C                   eval      VATFGS = vlrpoi
011000170504     C                   eval      invfil = vlrpoi
011100170504     C                   endif
011200010601     C*
011300010604     C* Ebbene...
011400010604     C*
011500010604     C  N31              ADD       1             §CTROK            7 0
011600010604     C   32              ADD       1             §CTRMO            7 0
011700010604     C   31              ADD       1             §CTRNO            7 0
011800110418     C*
011900110418     C* Verifico se sui parametri del traduttore è richiesta l'esclusione del 1020005
012000110418     C                   IF        VATCCM = 1020005 and %subst(vlrppt:2:1) = 'S'
012100110418     C                   ELSE
012200020916     C  N31              WRITE     FIVAT000
012300110418     C                   ENDIF
012400010604     C*
012500010604     C                   if        *in31 = *off and
012600010604     C                             *in32 = *off
012700010604     C                   eval      vinflg = '1'
012800010604     C                   else
012900010604     C                   eval      vinflg = '2'
013000010604     C                   endif
013100010604     C                   endif
013200010604     C*
013300010330     C                   else
013400010330     C                   eval      vinflg = '1'
013500010330     C                   endif
013600010601     C*
013700010601     C  N70              update    tivin000
013800010330     C*
013900010330     C  N70              ENDdo
014000010601     C
014100010601     C*
014200010330     C* Se non ci sono record con errori ...
014300010601     C                   if        §ctrno = 0 and
014400170504     C                             §ctrmo = 0
014500010330     C* ... restituisco esito OK.
014600010330     C                   eval      wrkesito = '0'
014700010330     C                   else
014800010330     C                   if        §ctrok > 0
014900010330     C                   eval      wrkesito = '1'
015000010330     C                   else
015100010615     C                   if        flgOk = '0'
015200010615     C                   eval      wrkesito = '2'
015300010615     C                   else
015400010615     C                   eval      wrkesito = '6'
015500010615     C                   endif
015600010330     C                   endif
015700010330     C                   endif
015800010330     C*
015900010330     C                   if        %open(tivin00r)
016000010330     C                   close     tivin00r
016100010330     C                   endif
016200020916     C                   if        %open(fivatwwr)
016300020916     C                   close     fivatwwr
016400010330     C                   endif
016500010601     C*
016600010601     C                   if        vlrpoi <> 999
016700010601     C                   eval      invfil = vlrpoi
016800010601     C                   endif
016900010330     C*
017000010330     C                   if        §ctrok > 0
017100010601     C                             and invfil > *zeros
017200010330     C                   exsr      invio
017300010330     C                   endif
017400010330     C*
017500010330     C                   ENDSR
017600010330     C***
017700010330
017800010601
017900010601
018000010601
018100010601
018200010330     C*----------------------------------------------------*
018300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
018400010330     C*----------------------------------------------------*
018500010330     C     INZVAR        BEGSR
018600010330     C*
018700010330     C*
018800010330     C                   ENDSR
018900010330     C*----------------------------------------------------*
019000010330     C*  IMPOSTAZIONE CAMPI COSTANTI
019100010330     C*----------------------------------------------------*
019200010330     C     DEFCAM        BEGSR
019300010330     C*
019400020916     C                   CLEAR                   FIVAT000
019500010330     C*
019600010330     C                   ENDSR
019700010330     C*----------------------------------------------------*
019800020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
019900010330     C*----------------------------------------------------*
020000010612     C     IMPVAT        BEGSR
020100010330     C*
020200010604     C                   SETOFF                                       3132
020300010612     C*
020400010612     C                   SETOFF                                       303132
020500010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
020600010612     C                   TESTN                   a_vatccm             30
020700010612     C  N30              TESTN                   a_vatccm               30
020800010612     C  N30              TESTN                   a_vatccm                 30
020900010612     C                   MOVE      a_vatccm      W001A             1
021000010612     C     *IN30         IFEQ      *OFF
021100010612     C     W001A         ORLT      '0'
021200010612     C                   SETON                                        31
021300010612     C                   Z-ADD     *zeros        VATCCM
021400010612     C                   EVAL      vinmsg = 'VATCCM'
021500010612     C                   ELSE
021600010612     C                   MOVE      a_vatccm      VATCCM
021700010612     C     VATCCM        IFLE      *ZEROS
021800010612     C                   SETON                                        31
021900010612     C                   Z-ADD     *zeros        VATCCM
022000010612     C                   EVAL      vinmsg = 'VATCCM'
022100010612     C                   ENDIF
022200010612     C                   ENDIF
022300010612     C*
022400010612     C                   TESTN                   a_vatlnp             30
022500010612     C  N30              TESTN                   a_vatlnp               30
022600010612     C  N30              TESTN                   a_vatlnp                 30
022700010612     C                   MOVE      a_vatlnp      W001A             1
022800010612     C     *IN30         IFEQ      *OFF
022900010612     C     W001A         ORLT      '0'
023000010612     C                   SETON                                        31
023100010612     C                   Z-ADD     *zeros        VATLNP
023200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
023300021010     C                             + ' ' + 'VATLNP'
023400010612     C                   ELSE
023500010612     C                   MOVE      a_vatlnp      VATLNP
023600010612     C     VATLNP        IFLE      *ZEROS
023700010612     C                   SETON                                        31
023800010612     C                   Z-ADD     *zeros        VATLNP
023900010612     C                   EVAL      vinmsg = %trimr(vinmsg)
024000021010     C                             + ' ' + 'VATLNP'
024100010612     C                   ENDIF
024200010612     C                   ENDIF
024300100301     C*
024400100301     C                   TESTN                   a_vatfgs             30
024500100301     C  N30              TESTN                   a_vatfgs               30
024600100301     C  N30              TESTN                   a_vatfgs                 30
024700100301     C                   MOVE      a_vatfgs      W001A             1
024800100301     C     *IN30         IFEQ      *OFF
024900100301     C     W001A         ORLT      '0'
025000100301     C                   SETON                                        31
025100100301     C                   Z-ADD     *zeros        VATFGS
025200100301     C                   EVAL      vinmsg = %trimr(vinmsg)
025300100301     C                             + ' ' + 'VATFGS'
025400100301     C                   ELSE
025500100301     C                   MOVE      a_vatfgs      VATFGS
025600100301     C     VATFGS        IFLE      *ZEROS
025700100301     C                   SETON                                        31
025800100301     C                   Z-ADD     *zeros        VATFGS
025900100301     C                   EVAL      vinmsg = %trimr(vinmsg)
026000100301     C                             + ' ' + 'VATFGS'
026100100301     C                   ENDIF
026200100301     C                   ENDIF
026300010612     C*
026400010612     C                   TESTN                   a_vataas             30
026500010612     C  N30              TESTN                   a_vataas               30
026600010612     C  N30              TESTN                   a_vataas                 30
026700010612     C                   MOVE      a_vataas      W001A             1
026800010612     C     *IN30         IFEQ      *OFF
026900010612     C     W001A         ORLT      '0'
027000010612     C                   SETON                                        31
027100010612     C                   MOVEL(P)  datcor        VATAAS
027200010612     C                   EVAL      vinmsg = %trimr(vinmsg)
027300010612     C                             + ' ' + 'VATAAS'
027400010612     C                   ELSE
027500010612     C                   MOVE      a_vataas      VATAAS
027600010612     C                   ENDIF
027700010612     C*
027800010612     C                   TESTN                   a_vatnrs             30
027900010612     C  N30              TESTN                   a_vatnrs               30
028000010612     C  N30              TESTN                   a_vatnrs                 30
028100010612     C                   MOVE      a_vatnrs      W001A             1
028200010612     C     *IN30         IFEQ      *OFF
028300010612     C     W001A         ORLT      '0'
028400010612     C     W001A         ANDNE     *BLANK
028500010612     C                   SETON                                        31
028600010612     C                   Z-ADD     *zeros        VATNRS
028700010612     C                   EVAL      vinmsg = %trimr(vinmsg)
028800010612     C                             + ' ' + 'VATNRS'
028900010612     C                   ELSE
029000010612     C                   MOVE      a_vatnrs      VATNRS
029100010612     C                   ENDIF
029200010612     C*
029300010612     C                   TESTN                   a_vatnsp             30
029400010612     C  N30              TESTN                   a_vatnsp               30
029500010612     C  N30              TESTN                   a_vatnsp                 30
029600010612     C                   MOVE      a_vatnsp      W001A             1
029700010612     C     *IN30         IFEQ      *OFF
029800010612     C     W001A         ORLT      '0'
029900010612     C                   SETON                                        31
030000010612     C                   Z-ADD     *zeros        VATNSP
030100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
030200010612     C                             + ' ' + 'VATNSP'
030300010612     C                   ELSE
030400010612     C                   MOVE      a_vatnsp      VATNSP
030500010612     C                   ENDIF
030600010612     C*
030700010612     C*
030800010612     C* ALFABETICI
030900010612     C                   MOVE      a_vatatb      VATATB
031000010612     C                   MOVE      a_vattrc      VATTRC
031100131018     C                   IF        a_vattrc <> 'S'
031200020725     C                   EVAL      VATNOT = %trim(a_vatnot)
031300131023     C                   ELSE
031400131023     C                   EVAL      VATNOT = a_vatnot
031500131018     C                   ENDIF
031600010330     C*
031700010330     C                   ENDSR
031800010612     C*----------------------------------------------------*
031900010330
032000010330
032100010330
032200990920      /TITLE Invio dei dati al punto operativo.
032300000613     C     invio         BEGSR
032400990920     C*
032500990920     C                   reset                   dscmz
032600010601     C                   move      invfil        cmzdst
032700990920     C                   eval      cmzfld = vlrfou
032800990920     C                   eval      cmzmbd = vlrhdl
032900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
033000000710     C                   if        prmfir = *blanks
033100020916     C                   eval      cmzfla = 'FIVAT00F'
033200020916     C                   eval      cmzmba = 'FIVAT00F'
033300000710     C                   else
033400000710     C                   eval      cmzfla = prmfir
033500000710     C                   eval      cmzmba = prmfir
033600000710     C                   endif
033700990920     C                   eval      cmznrr = *zeros
033800990920     C                   move      §ctrok        cmznrr
033900021003     C                   eval      cmzlba = vlrfl1
034000990920     C                   call(e)   'TIS711C'
034100990920     C                   parm                    dscmz
034200990921     C                   parm      *blanks       esito
034300990920     C                   if        %error
034400990920     C                             or cmzerr = '1'
034500990921     C                             or esito  = '1'
034600000710     C                   eval      wrkesito = '3'
034700990920     C                   endif
034800990920     C*
034900000613     C                   ENDSR
035000990910
035100010601
035200010601
035300010601
035400010601
035500010601      /TITLE Invio dei dati al punto operativo.
035600010601     C     opeini        BEGSR
035700010601     C*
035800010601     C* Inizializzo flag e contatori operativi
035900010615     C                   movel     '0'           flgOk             1
036000010601     C                   z-add     *zeros        invfil            3 0
036100030407     C*
036200030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
036300030407     C                   if        %subst(vlrppt:1:1) = 'S'
036400030407     C                   seton                                        50
036500030407     C                   else
036600030407     C                   setoff                                       50
036700030407     C                   endif
036800010601     C*
036900010601     C                   ENDSR
037000010601     C***
037100170512
037200170512
037300170512
037400170512     C     chkVPC999     BEGSR
037500170512     C*
037600170512     C* Verifico se codice cliente già presente in schiera codici abilitati "ESW999"
037700170512     C     vatCCM        LOOKUP    skVPC999                               55
037800170512     C                   IF        %found
037900170512     C                   SETON                                        55
038000170512     C                   ELSE
038100170512     C                   SETOFF                                       55
038200170512     C*
038300170512     C* Cerco in tabella VPC se c'è la personalizzazione
038400170512     C                   movel(p)  'VPC'         tbeCOD
038500170512     C                   movel(p)  vatCCM        tbeKE1
038600170512     C     KEYtbe01_P    chain     tntbe01l
038700170512     C                   if        %found(tntbe01l)
038800170512     C                   movel(P)  tbeUNI        DVPC
038900170512     C                   if        §VPCESW999 = 'S'
039000170512     C                   add       1             jVPC
039100170512     C                   eval      skVPC999(jVPC) = vatCCM
039200170512     C                   SETON                                        55
039300170512     C                   endif
039400170512     C                   endif
039500170512     C                   ENDIF
039600170512     C*
039700170512     C                   ENDSR
039800170512     C*------------------------------------------------------------------------*
039900010601
040000010330
040100010330
040200000613     C     *inzsr        BEGSR
040300990910     C*
040400990910     C     *entry        plist
040500990920     C                   parm                    tivlrds
040600990921     C                   parm      wrkesito      esito
040700000724     C                   parm                    prmlit
040800000710     C                   parm                    prmfir
040900170512     C*
041000170512     C* Definizione chiavi di lettura
041100170512     C* TNTBE01L - parziale
041200170512     C     KEYtbe01_P    klist
041300170512     C                   kfld                    tbeCOD                         *UTENTE
041400170512     C                   kfld                    tbeKE1                         *CODICE
041500010330     C*
041600010330     C* CALCOLA LA DATA CORRENTE
041700100301     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
041800100301     C                   eval      datcor = %dec(%date() : *ISO)
041900000613     C*
042000000613     C                   ENDSR
042100000613     C***
