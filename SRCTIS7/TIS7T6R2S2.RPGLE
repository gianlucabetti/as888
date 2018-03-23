000100020916      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020916     FFIVATwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900010612     D*
002000010612     D fnvataar        ds                  inz
002100010612     D  a_vatatb               1      1
002200010612     D  a_vatccm               3      9
002300010612     D  a_vatlnp              11     13
002400010612     D  a_vataas              15     18
002500010612     D  a_vatnrs              20     21
002600010612     D  a_vatnsp              23     29
002700010612     D  a_vattrc              30     30
002800010612     D  a_vatnot              31     65
002900100301
003000100301     D*------------------
003100100301     D* DS FIVAT DA EASYWEB
003200100301     D*------------------
003300100301     D ds_easyweb      ds
003400100301     D   ds_vat                            like(fnvataar)
003500100301     D   a_filler1                    1
003600100301     D   a_vatfgs                          like(a_vatlnp)
003700010330
003800010330
003900010330
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300010601     C*
004400010601     C                   exsr      opeini
004500010612     C                   exsr      rwvat
004600010601     C*
004700010601     C                   seton                                        lr
004800010330
004900010601
005000010601
005100010330     C*--------------------------------------------------------
005200020916     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
005300010330     C*--------------------------------------------------------
005400010612     C     RWVAT         BEGSR
005500010330     C*
005600010330     C                   if        not %open(tivin00r)
005700010330     C                   open      tivin00r
005800010330     C                   endif
005900020916     C                   if        not %open(fivatwwr)
006000020916     C                   open      fivatwwr
006100010330     C                   endif
006200010330     C*
006300010604     C                   clear                   §CTROK
006400010604     C                   clear                   §CTRMO
006500010604     C                   clear                   §CTRNO
006600010330     C*
006700010330     C                   DO        *HIVAL
006800010330     C*
006900010330     C                   READ      tivin00r                               70
007000010618     C*
007100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
007200010618     C                   if        vinflg = '1'
007300010618     C                   eval      flgOk = '1'
007400010618     C                   endif
007500010618     C*
007600010330     C                   if        vindta > *blanks
007700010330     C                   add       1             rrnum
007800010330     C*
007900010601     C                   if        *in70 = *off and
008000010330     C                             (vinflg = *blanks
008100010330     C                              or vinflg = '0'
008200010330     C                              or vinflg = '2')
008300010711     C*
008400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
008500010711     C                   if        vinflg = *blanks or vinflg = '0'
008600010711     C                   clear                   vinmsg
008700010711     C                   endif
008800010711     C*
008900100301     C                   eval      ds_easyweb = vindta
009000100301     C                   eval      fnvataar   = ds_vat
009100010601     C*
009200010330     C                   exsr      inzvar
009300010330     C                   exsr      defcam
009400010612     C                   exsr      impvat
009500170504     C*
009600170504     C* Effettuo considerazioni x elaborazioni "multi-filiale"
009700170504     C                   if        vlrpoi = 999
009800170504     C                   if        VATFGS = 999
009900170504     C                   movel     VATCCM        VATFGS
010000170504     C                   endif
010100170504     C                   eval      invfil = VATFGS
010200170504     C                   else
010300170504     C                   eval      VATFGS = vlrpoi
010400170504     C                   eval      invfil = vlrpoi
010500170504     C                   endif
010600010601     C*
010700010604     C* Ebbene...
010800010604     C*
010900010604     C  N31              ADD       1             §CTROK            7 0
011000010604     C   32              ADD       1             §CTRMO            7 0
011100010604     C   31              ADD       1             §CTRNO            7 0
011200110418     C*
011300110418     C* Verifico se sui parametri del traduttore è richiesta l'esclusione del 1020005
011400110418     C                   IF        VATCCM = 1020005 and %subst(vlrppt:2:1) = 'S'
011500110418     C                   ELSE
011600020916     C  N31              WRITE     FIVAT000
011700110418     C                   ENDIF
011800010604     C*
011900010604     C                   if        *in31 = *off and
012000010604     C                             *in32 = *off
012100010604     C                   eval      vinflg = '1'
012200010604     C                   else
012300010604     C                   eval      vinflg = '2'
012400010604     C                   endif
012500010604     C                   endif
012600010604     C*
012700010330     C                   else
012800010330     C                   eval      vinflg = '1'
012900010330     C                   endif
013000010601     C*
013100010601     C  N70              update    tivin000
013200010330     C*
013300010330     C  N70              ENDdo
013400010601     C
013500010601     C*
013600010330     C* Se non ci sono record con errori ...
013700010601     C                   if        §ctrno = 0 and
013800170504     C                             §ctrmo = 0
013900010330     C* ... restituisco esito OK.
014000010330     C                   eval      wrkesito = '0'
014100010330     C                   else
014200010330     C                   if        §ctrok > 0
014300010330     C                   eval      wrkesito = '1'
014400010330     C                   else
014500010615     C                   if        flgOk = '0'
014600010615     C                   eval      wrkesito = '2'
014700010615     C                   else
014800010615     C                   eval      wrkesito = '6'
014900010615     C                   endif
015000010330     C                   endif
015100010330     C                   endif
015200010330     C*
015300010330     C                   if        %open(tivin00r)
015400010330     C                   close     tivin00r
015500010330     C                   endif
015600020916     C                   if        %open(fivatwwr)
015700020916     C                   close     fivatwwr
015800010330     C                   endif
015900010601     C*
016000010601     C                   if        vlrpoi <> 999
016100010601     C                   eval      invfil = vlrpoi
016200010601     C                   endif
016300010330     C*
016400010330     C                   if        §ctrok > 0
016500010601     C                             and invfil > *zeros
016600010330     C                   exsr      invio
016700010330     C                   endif
016800010330     C*
016900010330     C                   ENDSR
017000010330     C***
017100010330
017200010601
017300010601
017400010601
017500010601
017600010330     C*----------------------------------------------------*
017700010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
017800010330     C*----------------------------------------------------*
017900010330     C     INZVAR        BEGSR
018000010330     C*
018100010330     C*
018200010330     C                   ENDSR
018300010330     C*----------------------------------------------------*
018400010330     C*  IMPOSTAZIONE CAMPI COSTANTI
018500010330     C*----------------------------------------------------*
018600010330     C     DEFCAM        BEGSR
018700010330     C*
018800020916     C                   CLEAR                   FIVAT000
018900010330     C*
019000010330     C                   ENDSR
019100010330     C*----------------------------------------------------*
019200020916     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
019300010330     C*----------------------------------------------------*
019400010612     C     IMPVAT        BEGSR
019500010330     C*
019600010604     C                   SETOFF                                       3132
019700010612     C*
019800010612     C                   SETOFF                                       303132
019900010612     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
020000010612     C                   TESTN                   a_vatccm             30
020100010612     C  N30              TESTN                   a_vatccm               30
020200010612     C  N30              TESTN                   a_vatccm                 30
020300010612     C                   MOVE      a_vatccm      W001A             1
020400010612     C     *IN30         IFEQ      *OFF
020500010612     C     W001A         ORLT      '0'
020600010612     C                   SETON                                        31
020700010612     C                   Z-ADD     *zeros        VATCCM
020800010612     C                   EVAL      vinmsg = 'VATCCM'
020900010612     C                   ELSE
021000010612     C                   MOVE      a_vatccm      VATCCM
021100010612     C     VATCCM        IFLE      *ZEROS
021200010612     C                   SETON                                        31
021300010612     C                   Z-ADD     *zeros        VATCCM
021400010612     C                   EVAL      vinmsg = 'VATCCM'
021500010612     C                   ENDIF
021600010612     C                   ENDIF
021700010612     C*
021800010612     C                   TESTN                   a_vatlnp             30
021900010612     C  N30              TESTN                   a_vatlnp               30
022000010612     C  N30              TESTN                   a_vatlnp                 30
022100010612     C                   MOVE      a_vatlnp      W001A             1
022200010612     C     *IN30         IFEQ      *OFF
022300010612     C     W001A         ORLT      '0'
022400010612     C                   SETON                                        31
022500010612     C                   Z-ADD     *zeros        VATLNP
022600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
022700021010     C                             + ' ' + 'VATLNP'
022800010612     C                   ELSE
022900010612     C                   MOVE      a_vatlnp      VATLNP
023000010612     C     VATLNP        IFLE      *ZEROS
023100010612     C                   SETON                                        31
023200010612     C                   Z-ADD     *zeros        VATLNP
023300010612     C                   EVAL      vinmsg = %trimr(vinmsg)
023400021010     C                             + ' ' + 'VATLNP'
023500010612     C                   ENDIF
023600010612     C                   ENDIF
023700100301     C*
023800100301     C                   TESTN                   a_vatfgs             30
023900100301     C  N30              TESTN                   a_vatfgs               30
024000100301     C  N30              TESTN                   a_vatfgs                 30
024100100301     C                   MOVE      a_vatfgs      W001A             1
024200100301     C     *IN30         IFEQ      *OFF
024300100301     C     W001A         ORLT      '0'
024400100301     C                   SETON                                        31
024500100301     C                   Z-ADD     *zeros        VATFGS
024600100301     C                   EVAL      vinmsg = %trimr(vinmsg)
024700100301     C                             + ' ' + 'VATFGS'
024800100301     C                   ELSE
024900100301     C                   MOVE      a_vatfgs      VATFGS
025000100301     C     VATFGS        IFLE      *ZEROS
025100100301     C                   SETON                                        31
025200100301     C                   Z-ADD     *zeros        VATFGS
025300100301     C                   EVAL      vinmsg = %trimr(vinmsg)
025400100301     C                             + ' ' + 'VATFGS'
025500100301     C                   ENDIF
025600100301     C                   ENDIF
025700010612     C*
025800010612     C                   TESTN                   a_vataas             30
025900010612     C  N30              TESTN                   a_vataas               30
026000010612     C  N30              TESTN                   a_vataas                 30
026100010612     C                   MOVE      a_vataas      W001A             1
026200010612     C     *IN30         IFEQ      *OFF
026300010612     C     W001A         ORLT      '0'
026400010612     C                   SETON                                        31
026500010612     C                   MOVEL(P)  datcor        VATAAS
026600010612     C                   EVAL      vinmsg = %trimr(vinmsg)
026700010612     C                             + ' ' + 'VATAAS'
026800010612     C                   ELSE
026900010612     C                   MOVE      a_vataas      VATAAS
027000010612     C                   ENDIF
027100010612     C*
027200010612     C                   TESTN                   a_vatnrs             30
027300010612     C  N30              TESTN                   a_vatnrs               30
027400010612     C  N30              TESTN                   a_vatnrs                 30
027500010612     C                   MOVE      a_vatnrs      W001A             1
027600010612     C     *IN30         IFEQ      *OFF
027700010612     C     W001A         ORLT      '0'
027800010612     C     W001A         ANDNE     *BLANK
027900010612     C                   SETON                                        31
028000010612     C                   Z-ADD     *zeros        VATNRS
028100010612     C                   EVAL      vinmsg = %trimr(vinmsg)
028200010612     C                             + ' ' + 'VATNRS'
028300010612     C                   ELSE
028400010612     C                   MOVE      a_vatnrs      VATNRS
028500010612     C                   ENDIF
028600010612     C*
028700010612     C                   TESTN                   a_vatnsp             30
028800010612     C  N30              TESTN                   a_vatnsp               30
028900010612     C  N30              TESTN                   a_vatnsp                 30
029000010612     C                   MOVE      a_vatnsp      W001A             1
029100010612     C     *IN30         IFEQ      *OFF
029200010612     C     W001A         ORLT      '0'
029300010612     C                   SETON                                        31
029400010612     C                   Z-ADD     *zeros        VATNSP
029500010612     C                   EVAL      vinmsg = %trimr(vinmsg)
029600010612     C                             + ' ' + 'VATNSP'
029700010612     C                   ELSE
029800010612     C                   MOVE      a_vatnsp      VATNSP
029900010612     C                   ENDIF
030000010612     C*
030100010612     C*
030200010612     C* ALFABETICI
030300010612     C                   MOVE      a_vatatb      VATATB
030400010612     C                   MOVE      a_vattrc      VATTRC
030500131018     C                   IF        a_vattrc <> 'S'
030600020725     C                   EVAL      VATNOT = %trim(a_vatnot)
030700131023     C                   ELSE
030800131023     C                   EVAL      VATNOT = a_vatnot
030900131018     C                   ENDIF
031000010330     C*
031100010330     C                   ENDSR
031200010612     C*----------------------------------------------------*
031300010330
031400010330
031500010330
031600990920      /TITLE Invio dei dati al punto operativo.
031700000613     C     invio         BEGSR
031800990920     C*
031900990920     C                   reset                   dscmz
032000010601     C                   move      invfil        cmzdst
032100990920     C                   eval      cmzfld = vlrfou
032200990920     C                   eval      cmzmbd = vlrhdl
032300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
032400000710     C                   if        prmfir = *blanks
032500020916     C                   eval      cmzfla = 'FIVAT00F'
032600020916     C                   eval      cmzmba = 'FIVAT00F'
032700000710     C                   else
032800000710     C                   eval      cmzfla = prmfir
032900000710     C                   eval      cmzmba = prmfir
033000000710     C                   endif
033100990920     C                   eval      cmznrr = *zeros
033200990920     C                   move      §ctrok        cmznrr
033300021003     C                   eval      cmzlba = vlrfl1
033400990920     C                   call(e)   'TIS711C'
033500990920     C                   parm                    dscmz
033600990921     C                   parm      *blanks       esito
033700990920     C                   if        %error
033800990920     C                             or cmzerr = '1'
033900990921     C                             or esito  = '1'
034000000710     C                   eval      wrkesito = '3'
034100990920     C                   endif
034200990920     C*
034300000613     C                   ENDSR
034400990910
034500010601
034600010601
034700010601
034800010601
034900010601      /TITLE Invio dei dati al punto operativo.
035000010601     C     opeini        BEGSR
035100010601     C*
035200010601     C* Inizializzo flag e contatori operativi
035300010615     C                   movel     '0'           flgOk             1
035400010601     C                   z-add     *zeros        invfil            3 0
035500030407     C*
035600030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
035700030407     C                   if        %subst(vlrppt:1:1) = 'S'
035800030407     C                   seton                                        50
035900030407     C                   else
036000030407     C                   setoff                                       50
036100030407     C                   endif
036200010601     C*
036300010601     C                   ENDSR
036400010601     C***
036500010601
036600010601
036700010601
036800010330
036900010330
037000000613     C     *inzsr        BEGSR
037100990910     C*
037200990910     C     *entry        plist
037300990920     C                   parm                    tivlrds
037400990921     C                   parm      wrkesito      esito
037500000724     C                   parm                    prmlit
037600000710     C                   parm                    prmfir
037700010330     C*
037800010330     C* CALCOLA LA DATA CORRENTE
037900100301     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
038000100301     C                   eval      datcor = %dec(%date() : *ISO)
038100000613     C*
038200000613     C                   ENDSR
038300000613     C***
