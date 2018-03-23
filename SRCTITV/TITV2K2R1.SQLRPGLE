000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900000613     D rrnum           s              6  0 INZ(*zeros)
002000120403     D depdest         s             92    INZ(*blanks)
002100120403     D deprif          s              6    INZ(*blanks)
002200051209     D parccm          s              8    INZ(*blanks)
002300010202     D parmbr          s             10    INZ(*blanks)
002400010202     D paropz          s              1    INZ(*blanks)
002500010202     D chkcall         s              1    INZ(*blanks)
002600020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002700120403     D wRIF            s              6    INZ(*blanks)
002800020725
002900020725     D*------------------
003000051209     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003100020725     D*------------------
003200051209     D RECORDSETDS     DS                  INZ
003300051209     D  VINDTA                     2048
003400051209     D  VINFLG                        1
003500120403     D  VINDEST                      92
003600120403     D  VINRIF                        6
003700051209     D  VINRRN                        8  0
003800060109     D*------------------
003900060109     D* DS REPERIMENTO NUMERATORE
004000060109     D*------------------
004100060109     D trul33ds      e ds                  inz
004200060109     D*------------------
004300060109     D* DS ARCHITETTURA
004400060109     D*------------------
004500060109     D kpjba         e ds                  inz
004600060109     D*------------------
004700020725     D*
004800990908
004900010201
005000010201
005100000913     C                   reset                   rrnum
005200990921     C                   reset                   esito
005300120403     C                   reset                   wrkesito
005400000613     C*
005500051209     C                   EXSR      RWVAB                                        LETT/SCR. VAB
005600000613     C*
005700010202     C* Effettuo la chiamata al CLLE preposto
005800050128     C                   call(e)   'TITVVTC'
005900010202     C                   parm                    parccm
006000010202     C                   parm                    parmbr
006100010202     C                   parm      '2'           paropz
006200000616     C*
006300010201     C                   seton                                        LR
006400990908
006500000801
006600910830     C*--------------------------------------------------------
006700051209     C* RWVAB    LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
006800910830     C*--------------------------------------------------------
006900051209     C     RWVAB         BEGSR
007000990910     C*
007100990914     C                   if        not %open(tivin00r)
007200990908     C                   open      tivin00r
007300990914     C                   endif
007400021113     C                   if        not %open(fivabwwr)
007500021113     C                   open      fivabwwr
007600990914     C                   endif
007700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
007800020305     C                   exsr      prevat
007900010201     C*
008000010202     C                   if        chkcall = '0'
008100010202     C*
008200021113     C                   if        not %open(fivatwwr)
008300021113     C                   open      fivatwwr
008400010201     C                   endif
008500990910     C*
008600010201     C                   clear                   §CTROKVB          5 0
008700020305     C                   clear                   §CTROKVT          5 0
008800000801     C                   clear                   §CTRMO            5 0
008900000801     C                   clear                   §CTRNO            5 0
009000990910     C*
009100020725     C*
009200020725     C/EXEC SQL
009300020725     C+ declare C1 cursor for select
009400120403     C+ vindta, vinflg, substr(vindta, 203, 92) as dest,
009500120403     C+ substr(vindta, 446, 6) as rif,
009600051209     C+ rrn(tivin00r)
009700051209     C+ from tivin00r order by dest, rif
009800020725     C+ for read only
009900020725     C/END-EXEC
010000020725     C
010100020725     C/EXEC SQL
010200020725     C+ open C1
010300020725     C/END-EXEC
010400020725     C
010500020725     C/EXEC SQL
010600051209     C+ Fetch C1 into :RECORDSETDS
010700020725     C/END-EXEC
010800020725     C*
010900020725     C                   dow       sqlcod = *zeros
011000990913     C*
011100020725     C                   if        vindta > *blanks
011200000613     C                   add       1             rrnum
011300000801     C*
011400020725     C                   if        vinflg = *blanks
011500020725     C                             or vinflg = '0'
011600020725     C                             or vinflg = '2'
011700000801     C*
011800020725     C                   clear                   x_vinmsg
011900020725     C                   eval      x_vinflg = '1'
012000010305     C*
012100051209     C* A rottura d destinatario eseguo operazioni
012200051209     C                   if        depdest = *blanks                            => 1° giro
012300051209     C                   eval      depdest = VINDEST
012400051209     C                   eval      deprif  = VINRIF
012500020725     C                   clear                   tivinds
012600020725     C                   exsr      inzvar
012700020725     C                   exsr      defcam
012800020305     C                   exsr      impvab
012900051209     C                   exsr      addvab
013000010305     C                   else
013100051209     C                   if        depdest <> VINDEST                           => rottura
013200051209     C                   eval      depdest  = VINDEST                           => memorizz. new rot
013300051209     C                   eval      deprif  = VINRIF
013400020305     C                   exsr      wrivab
013500020725     C                   clear                   tivinds
013600020725     C                   exsr      inzvar
013700020725     C                   exsr      defcam
013800020305     C                   exsr      impvab
013900051209     C                   exsr      addvab
014000051209     C                   else                                                   => x stesso dest.
014100051209     C                   if        deprif  <> VINRIF                            => rottura
014200051209     C                   eval      deprif   = VINRIF                            => memorizz. new rot
014300051209     C                   exsr      addvab
014400051209     C                   endif
014500010305     C                   endif
014600010305     C                   endif
014700051209     C                   endif
014800051209     C*
014900051209     C* Eseguo routine x reperimento e scrittura "chi sono"
015000051209     C                   EXSR      WRIVAT
015100000905     C*
015200000905     C                   else
015300020725     C                   eval      x_vinflg = '1'
015400000905     C                   endif
015500000905     C*
015600020725     C     VINRRN        chain     tivin000
015700020725     C                   if        %found(tivin00r)
015800020725     C                   eval      y_vinflg = x_vinflg
015900020725     C                   eval      y_vinmsg = x_vinmsg
016000020725     C                   update    tivin000
016100020725     C                   endif
016200020725     C*
016300020725     C/EXEC SQL
016400051209     C+ Fetch C1 into :RECORDSETDS
016500020725     C/END-EXEC
016600020725     C*
016700020725     C                   enddo
016800020725     C*
016900020725     C/EXEC SQL
017000020725     C+ close C1
017100020725     C/END-EXEC
017200000905     C*
017300020305     C* Scarico i VAB rimasti "in sospeso"
017400020305     C                   exsr      wrivab
017500010202     C*
017600010202     C                   endif
017700990910
017800990910     C* Se non ci sono record con errori ...
017900000710     C                   if        §ctrno = 0
018000990910     C* ... restituisco esito OK.
018100990921     C                   eval      wrkesito = '0'
018200990910     C                   else
018300010201     C                   if        §ctrokvb > 0
018400990921     C                   eval      wrkesito = '1'
018500000710     C                   else
018600000710     C                   eval      wrkesito = '2'
018700990910     C                   endif
018800000710     C                   endif
018900990910     C*
019000990914     C                   if        %open(tivin00r)
019100990908     C                   close     tivin00r
019200990914     C                   endif
019300021113     C                   if        %open(fivabwwr)
019400021113     C                   close     fivabwwr
019500990914     C                   endif
019600021113     C                   if        %open(fivatwwr)
019700021113     C                   close     fivatwwr
019800010201     C                   endif
019900990910     C*
020000010201     C                   if        §ctrokvb > 0
020100000724     C                             and vlrpoi <> *zeros
020200010202     C                   exsr      invio
020300990920     C                   endif
020400990920     C*
020500910830     C                   ENDSR
020600000613     C***
020700010305
020800010305     C*----------------------------------------------------*
020900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
021000010305     C*----------------------------------------------------*
021100051222     C     WRIVAB        BEGSR
021200051222     C*
021300051222     C* Considerazioni sul contenuto di campi precedentemente valorizzati
021400051222     C                   IF        FlgCAS <> '0'
021500051222     C                   IF        VABCBO = '1'
021600051222     C                   EVAL      VABCBO = '4'
021700051222     C                   ELSE
021800051222     C                   EVAL      VABCBO = '6'
021900051222     C                   ENDIF
022000051222     C                   ENDIF
022100051222     C*
022200051222     C* Eseguo routine finale x considerazioni specifiche su importi/divise
022300051222     C                   EXSR      CHKIMPDIV
022400051222     C*
022500051222     C* Ebbene...
022600051222     C                   ADD       1             §CTRMO
022700051222     C                   IF        errore <> *zeros
022800051222     C                   ADD       1             §CTRNO
022900051222     C                   EVAL      x_vinflg = '2'
023000051222     C                   ELSE
023100051222     C                   ADD       1             §CTROKVB
023200051222     C                   ENDIF
023300010305     C*
023400021113     C                   write     fivab000                                     => scarico il VAB
023500010305     C*
023600010305     C                   ENDSR
023700990920
023800000801     C*----------------------------------------------------*
023900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024000000801     C*----------------------------------------------------*
024100010201     C     INZVAR        BEGSR
024200000801     C*
024300051209     C                   Z-ADD     *zeros        Num5_0            5 0
024400051209     C                   Z-ADD     *zeros        wGiro             8 0
024500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024600020725     C                   MOVEL     '0'           FlgCAS            1
024700000801     C*
024800000801     C                   ENDSR
024900000801     C*----------------------------------------------------*
025000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025100000801     C*----------------------------------------------------*
025200020904     C     DEFCAM        BEGSR
025300000801     C*
025400021113     C                   CLEAR                   FIVAB000
025500021113     C                   CLEAR                   FIVAT000
025600120403     C                   Z-ADD     0181933       VABCCM
025700120403     C                   Z-ADD     0181933       VATCCM
025800120403     C                   Z-ADD     018           VABLNP
025900120403     C                   Z-ADD     018           VATLNP
026000010201     C                   Z-ADD     000           VABCTR
026100020305     C                   MOVEL     '7Q'          VABCTM
026200051209     C                   MOVEL     'C'           VABTSP
026300051209     C                   MOVEL     '1'           VABCBO
026400020305     C                   MOVEL     'E'           VATTRC
026500051212     C* ... e poi verifico se sono stati passati come parametri
026600051212     C                   IF        vlrppt > *blanks
026700051212     C                   IF        %subst(vlrppt:1:7) <> *blanks
026800051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026900051212     C                   EXSR      CHKNUM
027000051212     C                   IF        PiInt=*on
027100051212     C                   Z-ADD     PiVal         VABCCM
027200051212     C                   Z-ADD     PiVal         VATCCM
027300051212     C                   ENDIF
027400051212     C                   ENDIF
027500051212     C                   IF        %subst(vlrppt:8:3) <> *blanks
027600051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027700051212     C                   EXSR      CHKNUM
027800051212     C                   IF        PiInt=*on
027900051212     C                   Z-ADD     PiVal         VABLNP
028000051212     C                   Z-ADD     PiVal         VATLNP
028100051212     C                   ENDIF
028200051212     C                   ENDIF
028300051212     C                   IF        %subst(vlrppt:11:3) <> *blanks
028400051212     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028500051212     C                   EXSR      CHKNUM
028600051212     C                   IF        PiInt=*on
028700051212     C                   Z-ADD     PiVal         VABCTR
028800051212     C                   ENDIF
028900051212     C                   ENDIF
029000051212     C                   IF        %subst(vlrppt:14:2) <> *blanks
029100051212     C                   EVAL      VABCTM = %subst(vlrppt:14:2)
029200051212     C                   ENDIF
029300051212     C                   ENDIF
029400000801     C*
029500000801     C                   ENDSR
029600000801     C*----------------------------------------------------*
029700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029800000801     C*----------------------------------------------------*
029900010201     C     IMPVAB        BEGSR
030000010305     C*
030100000801     C                   Z-ADD     *zeros        errore            1 0
030200021113     C*
030300021113     C                   MOVE(P)   vlrpoi        VABFGS
030400021113     C                   MOVE(P)   vlrpoi        VATFGS
030500051209     C*
030600051209     C* Valorizzo i campi data spedizione
030700051209     C                   MOVEL     datcor        VABAAS
030800051209     C                   MOVEL     datcor        VATAAS
030900051209     C                   MOVE      datcor        VABMGS
031000020725     C*
031100020725     C* Campi relativi al destinatario
031200051209     C                   EVAL      VABRSD=%trim(%subst(vindta:203:30))
031300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
031400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
031500020117     C* ==
031600051209     C                   EVAL      VABIND=%trim(%subst(vindta:233:25))
031700051209     C                   EVAL      VABLOD=%trim(%subst(vindta:258:30))
031800051209     C                   EVAL      VABPRD=%trim(%subst(vindta:293:2))
031900051212     C                   EVAL      VABRMA=%trim(%subst(vindta:446:10))
032000051209     C                   IF        %subst(vindta:1130:1) = 'E'
032100051209     C                   EVAL      VABTSP='E'
032200051209     C                   ENDIF
032300051209     C                   IF        %subst(vindta:46:2) = '02'
032400051209     C                   EVAL      VABCBO='2'
032500051209     C                   ENDIF
032600051209     C                   MOVEL     *zeros        wAlfaDCR          8
032700051219     C                   EVAL      wAlfaDCR=%trim(%subst(vindta:1095:8))
032800051209     C                   MOVE(P)   wAlfaDCR      VABDCR
032900020725     C*
033000020725     C* Campi numerici
033100051209     C*
033200060109     C* ...VABNSP/VATNSP => Stacco un numeratore da AZNUM
033300060109     C                   clear                   TRUL33DS
033400060109     C                   eval      I33OPE = *zeros
033500060109     C                   eval      I33CNU = 302
033600060109     C                   eval      I33NUM = 1
033700060109     C                   movel     TRUL33DS      KPJBU
033800060109     C                   call      'TRUL33R'
033900060109     C                   parm                    KPJBA
034000060109     C                   movel     KPJBU         TRUL33DS
034100060109     C                   if        O33ERR = *zeros
034200060109     C                   z-add     O33NRF        VABNSP
034300060109     C                   z-add     O33NRF        VATNSP
034400060109     C                   else
034500060109     C                   Z-ADD     1             errore
034600060109     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
034700060109     C                             + ' ' + 'VABNSP VATNSP'
034800060109     C                   endif
034900060109     C* ...VABRMN
035000120403     C                   EVAL      PiStr=%trim(%subst(vindta:446:6))
035100051209     C                   EXSR      CHKNUM
035200051209     C                   IF        PiInt=*on
035300051209     C                   Z-ADD     PiVal         VABRMN
035400051209     C                   ELSE
035500051209     C                   ADD       1             errore
035600051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035700060109     C                             + ' ' + 'VABRMN'
035800051209     C                   ENDIF
035900051209     C* ...VABCAD
036000051209     C                   EVAL      PiStr=%trim(%subst(vindta:288:5))
036100000801     C                   EXSR      CHKNUM
036200000801     C                   IF        PiInt=*on
036300000801     C                   Z-ADD     PiVal         Num5_0
036400010208     C                   MOVEL(p)  Num5_0        VABCAD
036500000801     C                   ELSE
036600000801     C                   ADD       1             errore
036700020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036800000801     C                             + ' ' + 'VABCAD'
036900000801     C                   ENDIF
037000000801     C*
037100000801     C                   ENDSR
037200051209     C*----------------------------------------------------*
037300051209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
037400051209     C*----------------------------------------------------*
037500051209     C     ADDVAB        BEGSR
037600051209     C*
037700051209     C* Incremento il contatore dei record elaborati x medesimo destinatario
037800051209     C                   ADD       1             wGiro
037900051209     C* ...VABNOT/VABNT2
038000051209     C                   IF        wGiro > 1
038100120403     C                   EVAL      wRIF = %trim(%subst(vindta:446:6))
038200051209     C                   IF        %len(%trim(VABNOT)) <=
038300051209     C                             %size(VABNOT) - %size(wRIF)
038400051209     C                   EVAL      VABNOT = %trim(VABNOT) + '-' + wRIF
038500051209     C                   ELSE
038600051209     C                   EVAL      VABNT2 = %trim(VABNT2) + '-' + wRIF
038700051209     C                   ENDIF
038800051209     C                   ENDIF
038900051209     C* ...VABNCL
039000051209     C                   EVAL      PiStr=%trim(%subst(vindta:335:4))
039100051209     C                   EXSR      CHKNUM
039200051209     C                   IF        PiInt=*on
039300051209     C                   ADD       PiVal         VABNCL
039400051209     C                   ELSE
039500051209     C                   ADD       1             errore
039600051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039700051209     C                             + ' ' + 'VABNCL'
039800051209     C                   ENDIF
039900051209     C* ...VABPKB
040000051209     C                   EVAL      PiStr=%trim(%subst(vindta:371:8))
040100051209     C                   EXSR      CHKNUM
040200051209     C                   IF        PiNum=*on
040300051209     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
040400051209     C                   ADD(H)    PiVal         VABPKB
040500051209     C                   ELSE
040600051209     C                   ADD       1             errore
040700051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800051209     C                             + ' ' + 'VABPKB'
040900051209     C                   ENDIF
041000051209     C* ...VABCAS
041100051209     C                   IF        %trim(%subst(vindta:681:10)) <> *zeros
041200051209     C                   EVAL      FlgCAS='1'
041300051209     C                   EVAL      VABVCA='EUR'
041400051209     C                   EVAL      PiStr=%trim(%subst(vindta:681:10))
041500051209     C                   EXSR      CHKNUM
041600051209     C                   IF        PiNum=*on
041700051209     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 decim.
041800051209     C                   ADD(H)    PiVal         VABCAS
041900051209     C                   ELSE
042000051209     C                   ADD       1             errore
042100051209     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042200051209     C                             + ' ' + 'VABCAS'
042300051209     C                   ENDIF
042400051209     C                   ENDIF
042500051209     C*
042600051209     C                   ENDSR
042700010201     C*----------------------------------------------------*
042800021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042900010201     C*----------------------------------------------------*
043000051209     C     WRIVAT        BEGSR
043100010201     C*
043200021113     C* Valorizzo l buffer di scrittura del FIVAT
043300051209     C                   eval      VATNOT = %trim(%subst(vindta:1:45))
043400021113     C                   write     FIVAT000
043500010201     C*
043600010201     C                   ENDSR
043700020904
043800020904
043900010202     C*----------------------------------------------------*
044000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044100010202     C*----------------------------------------------------*
044200020305     C     PREVAT        BEGSR
044300010202     C*
044400021113     C* Compongo il nome del membro da dare al FIVATWWR
044500010202     C                   eval      parmbr = vlrhdl
044600010202     C                   movel     'M'           parmbr
044700051209     C                   eval      parccm = vlrksc
044800010202     C                   eval      paropz = '1'
044900010202     C* Effettuo la chiamata al CLLE preposto
045000050128     C                   call(e)   'TITVVTC'
045100010202     C                   parm                    parccm
045200010202     C                   parm                    parmbr
045300010202     C                   parm                    paropz
045400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045500010202     C                   if        %error
045600010202     C                   movel     '1'           chkcall
045700010202     C                   else
045800010202     C                   movel     '0'           chkcall
045900010202     C                   endif
046000010202     C*
046100010202     C                   ENDSR
046200000801     C*----------------------------------------------------*
046300000801     C*  CONTROLLO NUMERICITA' CAMPI
046400000801     C*----------------------------------------------------*
046500000801     C     CHKNUM        BEGSR
046600000801     C*
046700000801     C                   call(e)   'ISNUMERIC'
046800000801     C                   PARM                    PiStr            30
046900000801     C                   PARM      ','           PiDecChr          1
047000000801     C                   PARM      *ZEROS        PiVal            30 9
047100000801     C                   PARM      '0'           PiInt             1
047200000801     C                   PARM      '0'           PiNum             1
047300000801     C                   IF        %error
047400000801     C                   EVAL      PiInt=*off
047500000801     C                   ENDIF
047600000801     C*
047700000801     C                   ENDSR
047800000801     C***
047900000801
048000011113
048100011113     C*----------------------------------------------------*
048200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
048300011113     C*----------------------------------------------------*
048400011113     C     CHKIMPDIV     BEGSR
048500011113     C*
048600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048700011113     C                   Z-ADD     *zeros        wrkDec            9 9
048800011113     C*
048900011113     C* Come prima cosa effettuo considerazioni sulla divisa
049000011113     C                   IF        vabIAS > *zeros
049100011113     C                   IF        vabVAS <> 'EUR'
049200011113     C                   EVAL      vabVAS =  'ITL'
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C                   IF        vabCAS > *zeros
049700011113     C                   IF        vabVCA <> 'EUR'
049800011113     C                   EVAL      vabVCA =  'ITL'
049900011113     C                   ENDIF
050000011113     C                   ENDIF
050100011113     C*
050200011113     C                   IF        vabVMD > *zeros
050300020305     C                   IF        vabVAD <> 'EUR'
050400011113     C                   EVAL      vabVAD =  'ITL'
050500011113     C                   ENDIF
050600011113     C                   ENDIF
050700011113     C*
050800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050900011113     C                   Z-ADD     vabIAS        wrkDec
051000011113     C                   IF        wrkDec > *zeros
051100011113     C                   IF        vabVAS = 'ITL'
051200011113     C                   EVAL      vabIAS = *zeros
051300011113     C                   ENDIF
051400011113     C                   ENDIF
051500011113     C*
051600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051700011113     C                   Z-ADD     vabCAS        wrkDec
051800011113     C                   IF        wrkDec > *zeros
051900011113     C                   IF        vabVCA = 'ITL'
052000011113     C                   EVAL      vabCAS = *zeros
052100011113     C                   ENDIF
052200011113     C                   ENDIF
052300011113     C*
052400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052500011113     C                   Z-ADD     vabVMD        wrkDec
052600011113     C                   IF        wrkDec > *zeros
052700011113     C                   IF        vabVAD = 'ITL'
052800011113     C                   EVAL      vabVMD = *zeros
052900011113     C                   ENDIF
053000011113     C                   ENDIF
053100011113     C*
053200011113     C                   ENDSR
053300011113     C***
053400011113
053500011113
053600000801
053700000801
053800990920      /TITLE Invio dei dati al punto operativo.
053900010202     C     invio         BEGSR
054000990920     C*
054100021113     C* 1° invio FIVAT
054200010201     C                   reset                   dscmz
054300010201     C                   move      vlrpoi        cmzdst
054400021113     C                   eval      cmzfld = 'FIVATWWR'
054500010201     C                   eval      cmzmbd = vlrhdl
054600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054700021009     C***                if        prmfir = *blanks
054800021113     C                   eval      cmzfla = 'FIVAT00F'
054900021113     C                   eval      cmzmba = 'FIVAT00F'
055000021009     C***                else
055100021009     C***                eval      cmzfla = prmfir
055200021009     C***                eval      cmzmba = prmfir
055300021009     C***                endif
055400010201     C                   eval      cmznrr = *zeros
055500020305     C                   move      §ctrokvt      cmznrr
055600021018     C                   eval      cmzlba = vlrfl1
055700010201     C                   call(e)   'TIS711C'
055800010201     C                   parm                    dscmz
055900010201     C                   parm      *blanks       esito
056000010205     C                   if        %error
056100010205     C                             or cmzerr = '1'
056200010205     C                             or esito  = '1'
056300010205     C                   eval      wrkesito = '3'
056400010205     C                   else
056500010201     C*
056600021113     C* 2° invio FIVAB
056700010201     C                   reset                   dscmz
056800010201     C                   move      vlrpoi        cmzdst
056900010201     C                   eval      cmzfld = vlrfou
057000010201     C                   eval      cmzmbd = vlrhdl
057100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
057200021009     C***                if        prmfir = *blanks
057300021113     C                   eval      cmzfla = 'FIVAB00F'
057400021113     C                   eval      cmzmba = 'FIVAB00F'
057500021009     C***                else
057600021009     C***                eval      cmzfla = prmfir
057700021009     C***                eval      cmzmba = prmfir
057800021009     C***                endif
057900010201     C                   eval      cmznrr = *zeros
058000010201     C                   move      §ctrokvb      cmznrr
058100021018     C                   eval      cmzlba = vlrfl1
058200010201     C                   call(e)   'TIS711C'
058300010201     C                   parm                    dscmz
058400010201     C                   parm      *blanks       esito
058500010201     C                   if        %error
058600010201     C                             or cmzerr = '1'
058700010201     C                             or esito  = '1'
058800010201     C                   eval      wrkesito = '3'
058900010201     C                   endif
059000010205     C                   endif
059100990920     C*
059200000613     C                   ENDSR
059300000613     C***
059400070411
059500070411     C     *pssr         BEGSR
059600070411     C*
059700070411     C                   if        %open(tivin00r)
059800070411     C                   close     tivin00r
059900070411     C                   endif
060000070411     C                   if        %open(fivabwwr)
060100070411     C                   close     fivabwwr
060200070411     C                   endif
060300070411     C                   if        %open(fivatwwr)
060400070411     C                   close     fivatwwr
060500070411     C                   endif
060600070411     C*
060700070411     C* Effettuo la chiamata al CLLE preposto
060800070411     C                   call(e)   'TITVVTC'
060900070411     C                   parm                    parccm
061000070411     C                   parm                    parmbr
061100070411     C                   parm      '2'           paropz
061200070411     C*
061300070411     C                   eval      wrkesito = '2'
061400070411     C*
061500070411     C                   seton                                        LR
061600070411     C*
061700070411     C                   ENDSR     '*CANCL'
061800070411     C***
061900990910
062000000613     C     *inzsr        BEGSR
062100990910     C*
062200990910     C     *entry        plist
062300990920     C                   parm                    tivlrds
062400990921     C                   parm      wrkesito      esito
062500000724     C                   parm                    prmlit
062600000710     C                   parm                    prmfir
062700000613     C*
062800000830     C* CALCOLA LA DATA CORRENTE
062900120403     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063000120403     C                   eval      datcor = %dec(%date() : *ISO)
063100000830     C*
063200000613     C                   ENDSR
063300000613     C***
