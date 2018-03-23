000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081027     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400081105     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500021017     FFIVABwwr  O    E             DISK    usropn
000600021017     FFIVATwwr  O    E             DISK    usropn
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
002000110207     D depspe          s             10    INZ(*blanks)
002100110207     D curspe          s             10    INZ(*blanks)
002200081027     D cntspe          s              7  0 INZ(*zeros)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700081105     D  Num5_0         s              5  0
002800081105     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002900010213
003000081105
003100081105     D*------------------
003200081105     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003300081105     D*------------------
003400081105     D INPUT_DS        DS                  INZ
003500081105     D  VINDTA                 1   2048
003600081105     D  VINFLG              2049   2049
003700081105     D  VINSPE              2050   2059
003800081105     D  VINRRN              2060   2067  0
003900081105     D*
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900051229     D*------------------
005000051229     D* DS REPERIMENTO NUMERATORE
005100051229     D*------------------
005200051229     D trul33ds      e ds                  inz
005300051229     D*------------------
005400051229     D* DS ARCHITETTURA
005500051229     D*------------------
005600051229     D kpjba         e ds                  inz
005700051229     D*------------------
005800081027
005900081027     D*------------------
006000081027     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006100081027     D*------------------
006200081027     D PiStr           S            100A   INZ
006300081027     D PiDecChr        S              1A   INZ
006400081027     D PiVal           S             63S30 INZ
006500081027     D PiInt           S               N   INZ
006600081027     D PiNum           S               N   INZ
006700081027
006800081027     D*------------------
006900081027     D* LINKING A DEFINIZIONI ESTERNE
007000081027     D*------------------
007100081027     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007200081027
007300990908
007400010201
007500010201
007600081105     C*
007700081105     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007800081105     C
007900081105     C/EXEC SQL
008000081105     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008100081105     C/END-EXEC
008200081105     C
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000613     C*
008700030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
008800000613     C*
008900010202     C* Effettuo la chiamata al CLLE preposto
009000031201     C                   call(e)   'TITVVTC'
009100010202     C                   parm                    parccm
009200010202     C                   parm                    parmbr
009300010202     C                   parm      '2'           paropz
009400000616     C*
009500010201     C                   seton                                        LR
009600990908
009700000801
009800910830     C*--------------------------------------------------------
009900030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
010000910830     C*--------------------------------------------------------
010100030226     C     RWFILE        BEGSR
010200990910     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600021017     C                   if        not %open(fivabwwr)
010700021017     C                   open      fivabwwr
010800990914     C                   endif
010900021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011000020305     C                   exsr      prevat
011100010201     C*
011200010202     C                   if        chkcall = '0'
011300010202     C*
011400021017     C                   if        not %open(fivatwwr)
011500021017     C                   open      fivatwwr
011600010201     C                   endif
011700990910     C*
011800010201     C                   clear                   §CTROKVB          5 0
011900020305     C                   clear                   §CTROKVT          5 0
012000000801     C                   clear                   §CTRMO            5 0
012100000801     C                   clear                   §CTRNO            5 0
012200081105     C*
012300081105     C/EXEC SQL
012400081105     C+ declare C1 cursor for select
012500110207     C+ vindta, vinflg, substr(vindta, 1221, 10) as sped, rrn(tivin00r)
012600081105     C+ from tivin00r
012700081105     C+ order by sped
012800081105     C+ for read only
012900081105     C/END-EXEC
013000081105     C
013100081105     C/EXEC SQL
013200081105     C+ open C1
013300081105     C/END-EXEC
013400081105     C
013500081105     C/EXEC SQL
013600081105     C+ Fetch C1 into :INPUT_DS
013700081105     C/END-EXEC
013800081105     C*
013900081105     C                   dow       sqlcod = *zeros
014000081105     C*
014100081105     C                   if        vindta > *blanks
014200081105     C                   add       1             rrnum
014300081105     C*
014400081105     C                   if           vinflg = *blanks
014500081105     C                             or vinflg = '0'
014600081105     C                             or vinflg = '2'
014700000801     C*
014800081105     C                   clear                   x_vinmsg
014900081105     C                   eval      x_vinflg = '1'
015000030310     C*
015100030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015200110207     C                   eval      PiStr=%trim(%subst(vindta:1221:10))
015300030411     C                   movel(p)  PiStr         curspe
015400030411     C                   add       1             cntspe
015500030411     C*
015600030411     C                   if        cntspe = 1
015700030411     C* A rottura inizializzo campi di totalizzazione
015800030411     C                   clear                   fivab000
015900051229     C                   exsr      repnum                                       => calcolo NSP
016000030411     C*
016100030411     C                   exsr      impvab                                       => carico VAB
016200031201     C                   exsr      exevat                                       => carico VAT
016300030411     C                   eval      depspe = curspe
016400030411     C                   else
016500030411     C                   if        curspe <> depspe
016600030411     C                   exsr      wrivab                                       => scarico VAB
016700030411     C* A rottura inizializzo campi di totalizzazione
016800030411     C                   clear                   fivab000
016900051229     C                   exsr      repnum                                       => calcolo NSP
017000030411     C*
017100030411     C                   exsr      impvab                                       => carico VAB
017200031201     C                   exsr      exevat                                       => carico VAT
017300030411     C                   eval      depspe = curspe
017400030411     C                   else
017500030411     C                   exsr      impvab                                       => carico VAB
017600031201     C                   exsr      exevat                                       => carico VAT
017700030411     C                   endif
017800030411     C                   endif
017900000905     C*
018000000905     C                   else
018100081105     C                   eval      x_vinflg = '1'
018200000905     C                   endif
018300030411     C                   endif
018400081105     C*
018500081105     C     VINRRN        chain     tivin000
018600081105     C                   if        %found(tivin00r)
018700081105     C                   eval      y_vinflg = x_vinflg
018800081105     C                   eval      y_vinmsg = x_vinmsg
018900081105     C                   update    tivin000
019000081105     C                   endif
019100081105     C*
019200081105     C/EXEC SQL
019300081105     C+ Fetch C1 into :INPUT_DS
019400081105     C/END-EXEC
019500081105     C                   enddo
019600081105     C*
019700081105     C/EXEC SQL
019800081105     C+ close C1
019900081105     C/END-EXEC
020000081105     C*
020100030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
020200030430     C                   exsr      wrivab                                       => scarico VAB
020300010202     C*
020400010202     C                   endif
020500990910
020600990910     C* Se non ci sono record con errori ...
020700000710     C                   if        §ctrno = 0
020800990910     C* ... restituisco esito OK.
020900990921     C                   eval      wrkesito = '0'
021000990910     C                   else
021100010201     C                   if        §ctrokvb > 0
021200990921     C                   eval      wrkesito = '1'
021300000710     C                   else
021400000710     C                   eval      wrkesito = '2'
021500990910     C                   endif
021600000710     C                   endif
021700990910     C*
021800990914     C                   if        %open(tivin00r)
021900990908     C                   close     tivin00r
022000990914     C                   endif
022100021017     C                   if        %open(fivabwwr)
022200021017     C                   close     fivabwwr
022300990914     C                   endif
022400021017     C                   if        %open(fivatwwr)
022500021017     C                   close     fivatwwr
022600010201     C                   endif
022700021017     C*
022800021017     C                   if        vlrpoi <> 999
022900021017     C                   eval      vlrpoi = vlrpoi
023000021017     C                   endif
023100990910     C*
023200010201     C                   if        §ctrokvb > 0
023300021017     C                             and vlrpoi > *zeros
023400010202     C                   exsr      invio
023500990920     C                   endif
023600990920     C*
023700910830     C                   ENDSR
023800000613     C***
023900990920
024000000801     C*----------------------------------------------------*
024100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024200000801     C*----------------------------------------------------*
024300010201     C     INZVAR        BEGSR
024400000801     C*
024500010201     C                   Z-ADD     *zeros        Num5_0
024600030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024700030310     C                   MOVEL     '0'           FlgCAS            1
024800000801     C*
024900000801     C                   ENDSR
025000051229     C*----------------------------------------------------*
025100051229     C*  REPERIMENTO NUMERATORE X NSP (VAB e VAT)
025200051229     C*----------------------------------------------------*
025300051229     C     REPNUM        BEGSR
025400051229     C*
025500051229     C* NSP => Stacco un numeratore da AZNUM
025600051229     C                   clear                   TRUL33DS
025700051229     C                   eval      I33OPE = *zeros
025800051229     C                   eval      I33CNU = 302
025900051229     C                   eval      I33NUM = 1
026000051229     C                   movel     TRUL33DS      KPJBU
026100051229     C                   call      'TRUL33R'
026200051229     C                   parm                    KPJBA
026300051229     C                   movel     KPJBU         TRUL33DS
026400051229     C                   if        O33ERR = *zeros
026500051229     C                   z-add     O33NRF        VABNSP
026600051229     C                   z-add     O33NRF        VATNSP
026700051229     C                   else
026800051229     C                   Z-ADD     1             errore
026900081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
027000051229     C                             + ' ' + 'VABNSP VATNSP'
027100051229     C                   endif
027200051229     C*
027300051229     C                   ENDSR
027400000801     C*----------------------------------------------------*
027500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027600000801     C*----------------------------------------------------*
027700000801     C     DEFCAM        BEGSR
027800000801     C*
027900020619     C* Imposto i valori di default...
028000041130     C                   Z-ADD     0062615       VABCCM
028100041130     C                   Z-ADD     0062615       VATCCM
028200021017     C                   Z-ADD     vlrpoi        VABFGS
028300021017     C                   Z-ADD     vlrpoi        VATFGS
028400041130     C                   Z-ADD     006           VABLNP
028500041130     C                   Z-ADD     006           VATLNP
028600030226     C                   Z-ADD     000           VABCTR
028700021017     C                   MOVEL     '7Q'          VABCTM
028800020619     C* ... e poi verifico se sono stati passati come parametri
028900020619     C                   IF        vlrppt > *blanks
029000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029100020619     C                   EXSR      CHKNUM
029200020619     C                   IF        PiInt=*on
029300020619     C                   Z-ADD     PiVal         VABCCM
029400020619     C                   Z-ADD     PiVal         VATCCM
029500020619     C                   ENDIF
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABLNP
030000020619     C                   Z-ADD     PiVal         VATLNP
030100020619     C                   ENDIF
030200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030300020619     C                   EXSR      CHKNUM
030400020619     C                   IF        PiInt=*on
030500020619     C                   Z-ADD     PiVal         VABCTR
030600020619     C                   ENDIF
030700120613     C                   EVAL      VABTSP=%subst(vlrppt:14:1)
030800020619     C                   ENDIF
030900000801     C*
031000000801     C                   ENDSR
031100000801     C*----------------------------------------------------*
031200030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031300000801     C*----------------------------------------------------*
031400010201     C     IMPVAB        BEGSR
031500000801     C*
031600020305     C                   EXSR      INZVAR
031700020305     C                   EXSR      DEFCAM
031800010305     C*
031900030226     C                   Z-ADD     *zeros        errore            1 0
032000000830     C                   MOVEL     datcor        VABAAS
032100000830     C                   MOVE      datcor        VABMGS
032200030226     C*
032300081027     C                   EVAL      VABRSD=%trim(%subst(vindta:797:35))
032400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032600020117     C* ==
032700081027     C                   EVAL      VABRD2=%trim(%subst(vindta:832:35))
032800081027     C                   EVAL      VABIND=%trim(%subst(vindta:897:35))
032900081027     C                   EVAL      VABLOD=%trim(%subst(vindta:1009:35))
033000081027     C                   EVAL      VABPRD=%trim(%subst(vindta:1109:2))
033100081027     C                   EVAL      VABRMA=%trim(%subst(vindta:1713:10))
033200081027     C                   EVAL      VABNOT=%trim(%subst(vindta:1605:35))
033300081027     C                   EVAL      VABNT2=%trim(%subst(vindta:1640:35))
033400081027     C                   IF        %subst(vindta:1459:1) = 'E'
033500031201     C                   EVAL      VABTSP='E'
033600031201     C                   ENDIF
033700081027     C                   IF        %subst(vindta:1723:1) = '1'
033800081027     C                   EVAL      VABTC2='P'
033900081027     C                   ENDIF
034000081027     C                   IF        %subst(vindta:1724:1) = '1'
034100081027     C                   EVAL      VABTC1='A'
034200081027     C                   ENDIF
034300030226     C* CAP
034400081027     C                   EVAL      PiStr=%trim(%subst(vindta:997:9))
034500010201     C                   EXSR      CHKNUM
034600010201     C                   IF        PiInt=*on
034700010201     C                   Z-ADD     PiVal         Num5_0
034800021017     C                   MOVEL     Num5_0        VABCAD
034900010201     C                   ELSE
035000010201     C                   ADD       1             errore
035100081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
035200021017     C                             + ' ' + 'VABCAD'
035300010201     C                   ENDIF
035400051229     C* RMN
035500110207     C                   EVAL      PiStr=%trim(%subst(vindta:1224:7))
035600010201     C                   EXSR      CHKNUM
035700010201     C                   IF        PiInt=*on
035800031201     C                   Z-ADD     PiVal         VABRMN
035900010201     C                   ELSE
036000010201     C                   ADD       1             errore
036100081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036200051229     C                             + ' ' + 'VABRMN'
036300010201     C                   ENDIF
036400081027     C* DCR
036500081027     C                   IF        %subst(vindta:1705:8) <> *blanks
036600081027     C                   EVAL      PiStr=%trim(%subst(vindta:1709:4) +
036700081027     C                                         %subst(vindta:1707:2) +
036800081027     C                                         %subst(vindta:1705:2))
036900081027     C                   EXSR      CHKNUM
037000081027     C                   IF        PiInt=*on
037100081027     C                   Z-ADD     PiVal         VABDCR
037200081027     C                   ELSE
037300081027     C                   ADD       1             errore
037400081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037500081027     C                             + ' ' + 'VABDCR'
037600081027     C                   ENDIF
037700081027     C                   ENDIF
037800081027     C* NCL
037900081027     C                   EVAL      VABNCL = VABNCL + 1
038000030226     C* PKB
038100081027     C                   EVAL      PiStr=%trim(%subst(vindta:1537:9))
038200030107     C                   EXSR      CHKNUM
038300030107     C                   IF        PiNum=*on
038400090702     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 decim.
038500090702     C                   ADD       PiVal         VABPKB
038600030107     C                   ELSE
038700030107     C                   ADD       1             errore
038800081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038900030107     C                             + ' ' + 'VABPKB'
039000030107     C                   ENDIF
039100090320     C* VLB
039200090320     C                   EVAL      PiStr=%trim(%subst(vindta:1546:9))
039300090320     C                   EXSR      CHKNUM
039400090320     C                   IF        PiNum=*on
039500090702     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 decim.
039600090702     C                   ADD       PiVal         VABVLB
039700090320     C                   ELSE
039800090320     C                   ADD       1             errore
039900090320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040000090320     C                             + ' ' + 'VABVLB'
040100090320     C                   ENDIF
040200030310     C*
040300030310     C* Considerazioni finali su CBO/CAS
040400030310     C                   IF        FlgCAS = '1'
040500010205     C                   EVAL      VABCBO = '4'
040600010205     C                   ELSE
040700010205     C                   EVAL      VABCBO = '1'
040800010205     C                   ENDIF
040900020305     C*
041000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041100011113     C                   EXSR      CHKIMPDIV
041200010202     C*
041300000801     C* Ebbene...
041400000801     C                   ADD       1             §CTRMO
041500010201     C                   IF        errore <> *zeros
041600000801     C                   ADD       1             §CTRNO
041700081105     C                   EVAL      x_vinflg = '2'
041800000801     C                   ELSE
041900010201     C                   ADD       1             §CTROKVB
042000000801     C                   ENDIF
042100000801     C*
042200000801     C                   ENDSR
042300030226     C*----------------------------------------------------*
042400030226     C*  SCARICAMENTO BUFFER RECORDS VAB
042500030226     C*----------------------------------------------------*
042600030226     C     WRIVAB        BEGSR
042700030310     C*
042800030310     C                   write     FIVAB000
042900030226     C*
043000030226     C                   ENDSR
043100030226     C*----------------------------------------------------*
043200030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
043300030226     C*----------------------------------------------------*
043400031201     C     EXEVAT        BEGSR
043500030226     C*
043600030226     C                   EXSR      INZVAR
043700030226     C                   EXSR      DEFCAM
043800030226     C*
043900030226     C                   MOVEL     datcor        VATAAS
044000030226     C*
044100030415     C* Valorizzo il barcode
044200031201     C                   EVAL      VATTRC='E'
044300081027     C                   EVAL      VATNOT=%trim(%subst(vindta:1490:30))
044400031201     C                   exsr      wrivat                                       => scarico VAT
044500031201     C*
044600031201     C* Valorizzo il telefono destinatario (se valorizzato)
044700031201     C                   EVAL      VATTRC='B'
044800081027     C                   EVAL      VATNOT=%trim(%subst(vindta:1132:24))
044900081027     C                   exsr      wrivat                                       => scarico VAT
045000120514     C*
045100120514     C* Valorizzo l'email destinatario (se valorizzato)
045200120514     C                   IF        %subst(vindta:1156:50) <> *blanks
045300120514     C                   EVAL      VATTRC='I'
045400120514     C                   EVAL      VATNOT=%trim(%subst(vindta:1156:35))
045500120514     C                   IF        VATNOT<>*blanks
045600120514     C                   exsr      wrivat                                       => scarico VAT
045700120514     C                   ENDIF
045800120514     C                   EVAL      VATTRC='J'
045900120514     C                   IF        VATNOT<>*blanks
046000120514     C                   EVAL      VATNOT=%trim(%subst(vindta:1156+35:50-35))
046100120514     C                   exsr      wrivat                                       => scarico VAT
046200120514     C                   ENDIF
046300120514     C                   ENDIF
046400030226     C*
046500030226     C                   ENDSR
046600010201     C*----------------------------------------------------*
046700021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
046800010201     C*----------------------------------------------------*
046900020305     C     WRIVAT        BEGSR
047000010201     C*
047100081027     C                   if        vatNOT <> *blanks
047200021017     C                   write     FIVAT000
047300081027     C                   endif
047400010201     C*
047500010201     C                   ENDSR
047600010202     C*----------------------------------------------------*
047700021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047800010202     C*----------------------------------------------------*
047900020305     C     PREVAT        BEGSR
048000010202     C*
048100021017     C* Compongo il nome del membro da dare al FIVATWWR
048200010202     C                   eval      parmbr = vlrhdl
048300010202     C                   movel     'M'           parmbr
048400050317     C                   eval      parccm = '00062615'
048500010202     C                   eval      paropz = '1'
048600010202     C* Effettuo la chiamata al CLLE preposto
048700031201     C                   call(e)   'TITVVTC'
048800010202     C                   parm                    parccm
048900010202     C                   parm                    parmbr
049000010202     C                   parm                    paropz
049100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049200010202     C                   if        %error
049300010202     C                   movel     '1'           chkcall
049400010202     C                   else
049500010202     C                   movel     '0'           chkcall
049600010202     C                   endif
049700010202     C*
049800010202     C                   ENDSR
049900000801     C*----------------------------------------------------*
050000000801     C*  CONTROLLO NUMERICITA' CAMPI
050100000801     C*----------------------------------------------------*
050200000801     C     CHKNUM        BEGSR
050300000801     C*
050400081027     C                   IF        PiDecChr = *blanks
050500081027     C                   EVAL      PiDecChr = ','
050600081027     C                   ENDIF
050700081027     C*
050800081027     C                   callp     UBISNUM_Check(PiStr
050900081027     C                                          :PiDecChr
051000081027     C                                          :PiVal
051100081027     C                                          :PiNum
051200081027     C                                          :PiInt)
051300000801     C*
051400000801     C                   ENDSR
051500000801     C***
051600000801
051700011113
051800011113     C*----------------------------------------------------*
051900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052000011113     C*----------------------------------------------------*
052100011113     C     CHKIMPDIV     BEGSR
052200011113     C*
052300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052400011113     C                   Z-ADD     *zeros        wrkDec            9 9
052500011113     C*
052600011113     C* Come prima cosa effettuo considerazioni sulla divisa
052700011113     C                   IF        vabIAS > *zeros
052800011113     C                   IF        vabVAS <> 'EUR'
052900011113     C                   EVAL      vabVAS =  'ITL'
053000011113     C                   ENDIF
053100011113     C                   ENDIF
053200011113     C*
053300011113     C                   IF        vabCAS > *zeros
053400011113     C                   IF        vabVCA <> 'EUR'
053500011113     C                   EVAL      vabVCA =  'ITL'
053600011113     C                   ENDIF
053700011113     C                   ENDIF
053800011113     C*
053900011113     C                   IF        vabVMD > *zeros
054000020305     C                   IF        vabVAD <> 'EUR'
054100011113     C                   EVAL      vabVAD =  'ITL'
054200011113     C                   ENDIF
054300011113     C                   ENDIF
054400011113     C*
054500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
054600011113     C                   Z-ADD     vabIAS        wrkDec
054700011113     C                   IF        wrkDec > *zeros
054800011113     C                   IF        vabVAS = 'ITL'
054900011113     C                   EVAL      vabIAS = *zeros
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055400011113     C                   Z-ADD     vabCAS        wrkDec
055500011113     C                   IF        wrkDec > *zeros
055600011113     C                   IF        vabVCA = 'ITL'
055700011113     C                   EVAL      vabCAS = *zeros
055800011113     C                   ENDIF
055900011113     C                   ENDIF
056000011113     C*
056100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056200011113     C                   Z-ADD     vabVMD        wrkDec
056300011113     C                   IF        wrkDec > *zeros
056400011113     C                   IF        vabVAD = 'ITL'
056500011113     C                   EVAL      vabVMD = *zeros
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C                   ENDSR
057000011113     C***
057100021017
057200000801
057300000801
057400990920      /TITLE Invio dei dati al punto operativo.
057500010202     C     invio         BEGSR
057600990920     C*
057700021017     C* 1° invio FIVAT
057800010201     C                   reset                   dscmz
057900021017     C                   move      vlrpoi        cmzdst
058000021017     C                   eval      cmzfld = 'FIVATWWR'
058100010201     C                   eval      cmzmbd = vlrhdl
058200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058300021009     C***                if        prmfir = *blanks
058400021017     C                   eval      cmzfla = 'FIVAT00F'
058500021017     C                   eval      cmzmba = 'FIVAT00F'
058600021009     C***                else
058700021009     C***                eval      cmzfla = prmfir
058800021009     C***                eval      cmzmba = prmfir
058900021009     C***                endif
059000010201     C                   eval      cmznrr = *zeros
059100020305     C                   move      §ctrokvt      cmznrr
059200021018     C                   eval      cmzlba = vlrfl1
059300010201     C                   call(e)   'TIS711C'
059400010201     C                   parm                    dscmz
059500010201     C                   parm      *blanks       esito
059600010205     C                   if        %error
059700010205     C                             or cmzerr = '1'
059800010205     C                             or esito  = '1'
059900010205     C                   eval      wrkesito = '3'
060000010205     C                   else
060100010201     C*
060200021017     C* 2° invio FIVAB
060300010201     C                   reset                   dscmz
060400021017     C                   move      vlrpoi        cmzdst
060500010201     C                   eval      cmzfld = vlrfou
060600010201     C                   eval      cmzmbd = vlrhdl
060700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
060800021009     C***                if        prmfir = *blanks
060900021017     C                   eval      cmzfla = 'FIVAB00F'
061000021017     C                   eval      cmzmba = 'FIVAB00F'
061100021009     C***                else
061200021009     C***                eval      cmzfla = prmfir
061300021009     C***                eval      cmzmba = prmfir
061400021009     C***                endif
061500010201     C                   eval      cmznrr = *zeros
061600010201     C                   move      §ctrokvb      cmznrr
061700021018     C                   eval      cmzlba = vlrfl1
061800010201     C                   call(e)   'TIS711C'
061900010201     C                   parm                    dscmz
062000010201     C                   parm      *blanks       esito
062100010201     C                   if        %error
062200010201     C                             or cmzerr = '1'
062300010201     C                             or esito  = '1'
062400010201     C                   eval      wrkesito = '3'
062500010201     C                   endif
062600010205     C                   endif
062700990920     C*
062800000613     C                   ENDSR
062900000613     C***
063000070411
063100090702     C***  *pssr         BEGSR
063200070411     C*
063300090702     C*                  if        %open(tivin00r)
063400090702     C*                  close     tivin00r
063500090702     C*                  endif
063600090702     C*                  if        %open(fivabwwr)
063700090702     C*                  close     fivabwwr
063800090702     C*                  endif
063900090702     C*                  if        %open(fivatwwr)
064000090702     C*                  close     fivatwwr
064100090702     C*                  endif
064200070411     C*
064300070411     C* Effettuo la chiamata al CLLE preposto
064400090702     C*                  call(e)   'TITVVTC'
064500090702     C*                  parm                    parccm
064600090702     C*                  parm                    parmbr
064700090702     C*                  parm      '2'           paropz
064800090702     C*
064900090702     C*                  eval      wrkesito = '2'
065000090702     C*
065100090702     C*                  seton                                        LR
065200070411     C*
065300090702     C*                  ENDSR     '*CANCL'
065400070411     C***
065500070411
065600990910
065700000613     C     *inzsr        BEGSR
065800990910     C*
065900990910     C     *entry        plist
066000990920     C                   parm                    tivlrds
066100990921     C                   parm      wrkesito      esito
066200000724     C                   parm                    prmlit
066300000710     C                   parm                    prmfir
066400000613     C*
066500000830     C* CALCOLA LA DATA CORRENTE
066600000830     C                   time                    wn14             14 0
066700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
066800000830     C                   z-add     wn8           g08dat
066900000830     C                   z-add     *zeros        g08inv
067000000830     C                   movel     '0'           g08err
067100000830     C                   call      'XSRDA8'
067200000830     C                   parm                    wlbda8
067300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
067400000830     C*
067500000613     C                   ENDSR
067600000613     C***
