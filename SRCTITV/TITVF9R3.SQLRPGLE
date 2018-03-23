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
002200130409     D savNSP          s                   LIKE(vabNSP)
002300081027     D cntspe          s              7  0 INZ(*zeros)
002400010202     D parccm          s              8    INZ(*blanks)
002500010202     D parmbr          s             10    INZ(*blanks)
002600010202     D paropz          s              1    INZ(*blanks)
002700010202     D chkcall         s              1    INZ(*blanks)
002800081105     D  Num5_0         s              5  0
002900081105     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003000130406     D fivabds       e ds                  extname(fivab00f)
003100010213
003200081105
003300081105     D*------------------
003400081105     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003500081105     D*------------------
003600081105     D INPUT_DS        DS                  INZ
003700081105     D  VINDTA                 1   2048
003800081105     D  VINFLG              2049   2049
003900081105     D  VINSPE              2050   2059
004000081105     D  VINRRN              2060   2067  0
004100081105     D*
004200000830
004300051229     D*------------------
004400051229     D* DS REPERIMENTO NUMERATORE
004500051229     D*------------------
004600051229     D trul33ds      e ds                  inz
004700051229     D*------------------
004800051229     D* DS ARCHITETTURA
004900051229     D*------------------
005000051229     D kpjba         e ds                  inz
005100051229     D*------------------
005200081027
005300081027     D*------------------
005400081027     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
005500081027     D*------------------
005600081027     D PiStr           S            100A   INZ
005700081027     D PiDecChr        S              1A   INZ
005800081027     D PiVal           S             63S30 INZ
005900081027     D PiInt           S               N   INZ
006000081027     D PiNum           S               N   INZ
006100081027
006200081027     D*------------------
006300081027     D* LINKING A DEFINIZIONI ESTERNE
006400081027     D*------------------
006500081027     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600081027
006700990908
006800010201
006900010201
007000081105     C*
007100081105     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007200081105     C
007300081105     C/EXEC SQL
007400081105     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007500081105     C/END-EXEC
007600081105     C
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000000613     C*
008100030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
008200000613     C*
008300010202     C* Effettuo la chiamata al CLLE preposto
008400031201     C                   call(e)   'TITVVTC'
008500010202     C                   parm                    parccm
008600010202     C                   parm                    parmbr
008700010202     C                   parm      '2'           paropz
008800170801     C*
008900170801     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
009000170801     C                   if        *in53
009100170801     C                   call      'TIS7P2SR'
009200170801     C                   parm      'C'           pIn_Opz           1
009300170801     C                   parm                    tivlrds
009400170801     C                   parm                    FIVABDS
009500170801     C                   parm      *blanks       pIn_CdIdAz        2
009600170801     C                   parm      *blanks       pIn_MaskPDF      50
009700170801     C                   parm      *blanks       pOut_Esito        1
009800170801     C                   endif
009900000616     C*
010000010201     C                   seton                                        LR
010100990908
010200000801
010300910830     C*--------------------------------------------------------
010400030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
010500910830     C*--------------------------------------------------------
010600030226     C     RWFILE        BEGSR
010700990910     C*
010800990914     C                   if        not %open(tivin00r)
010900990908     C                   open      tivin00r
011000990914     C                   endif
011100021017     C                   if        not %open(fivabwwr)
011200021017     C                   open      fivabwwr
011300990914     C                   endif
011400021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011500020305     C                   exsr      prevat
011600010201     C*
011700010202     C                   if        chkcall = '0'
011800010202     C*
011900021017     C                   if        not %open(fivatwwr)
012000021017     C                   open      fivatwwr
012100010201     C                   endif
012200130406     C*
012300130406     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
012400130406     C                   if        *in53
012500130406     C                   call      'TIS7P2SR'
012600130406     C                   parm      'O'           pIn_Opz           1
012700130406     C                   parm                    tivlrds
012800130406     C                   parm                    FIVABDS
012900130406     C                   parm      *blanks       pIn_CdIdAz        2
013000130406     C                   parm      *blanks       pIn_MaskPDF      50
013100130406     C                   parm      *blanks       pOut_Esito        1
013200130406     C                   endif
013300990910     C*
013400010201     C                   clear                   §CTROKVB          5 0
013500020305     C                   clear                   §CTROKVT          5 0
013600000801     C                   clear                   §CTRMO            5 0
013700000801     C                   clear                   §CTRNO            5 0
013800081105     C*
013900081105     C/EXEC SQL
014000081105     C+ declare C1 cursor for select
014100110207     C+ vindta, vinflg, substr(vindta, 1221, 10) as sped, rrn(tivin00r)
014200081105     C+ from tivin00r
014300081105     C+ order by sped
014400081105     C+ for read only
014500081105     C/END-EXEC
014600081105     C
014700081105     C/EXEC SQL
014800081105     C+ open C1
014900081105     C/END-EXEC
015000081105     C
015100081105     C/EXEC SQL
015200081105     C+ Fetch C1 into :INPUT_DS
015300081105     C/END-EXEC
015400081105     C*
015500081105     C                   dow       sqlcod = *zeros
015600081105     C*
015700081105     C                   if        vindta > *blanks
015800081105     C                   add       1             rrnum
015900081105     C*
016000081105     C                   if           vinflg = *blanks
016100081105     C                             or vinflg = '0'
016200081105     C                             or vinflg = '2'
016300000801     C*
016400081105     C                   clear                   x_vinmsg
016500081105     C                   eval      x_vinflg = '1'
016600030310     C*
016700030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016800110207     C                   eval      PiStr=%trim(%subst(vindta:1221:10))
016900030411     C                   movel(p)  PiStr         curspe
017000030411     C                   add       1             cntspe
017100030411     C*
017200030411     C                   if        cntspe = 1
017300030411     C* A rottura inizializzo campi di totalizzazione
017400030411     C                   clear                   fivab000
017500051229     C                   exsr      repnum                                       => calcolo NSP
017600030411     C*
017700030411     C                   exsr      impvab                                       => carico VAB
017800031201     C                   exsr      exevat                                       => carico VAT
017900030411     C                   eval      depspe = curspe
018000030411     C                   else
018100030411     C                   if        curspe <> depspe
018200030411     C                   exsr      wrivab                                       => scarico VAB
018300030411     C* A rottura inizializzo campi di totalizzazione
018400030411     C                   clear                   fivab000
018500051229     C                   exsr      repnum                                       => calcolo NSP
018600030411     C*
018700030411     C                   exsr      impvab                                       => carico VAB
018800031201     C                   exsr      exevat                                       => carico VAT
018900030411     C                   eval      depspe = curspe
019000030411     C                   else
019100030411     C                   exsr      impvab                                       => carico VAB
019200031201     C                   exsr      exevat                                       => carico VAT
019300030411     C                   endif
019400030411     C                   endif
019500000905     C*
019600000905     C                   else
019700081105     C                   eval      x_vinflg = '1'
019800000905     C                   endif
019900030411     C                   endif
020000081105     C*
020100081105     C     VINRRN        chain     tivin000
020200081105     C                   if        %found(tivin00r)
020300081105     C                   eval      y_vinflg = x_vinflg
020400081105     C                   eval      y_vinmsg = x_vinmsg
020500081105     C                   update    tivin000
020600081105     C                   endif
020700081105     C*
020800081105     C/EXEC SQL
020900081105     C+ Fetch C1 into :INPUT_DS
021000081105     C/END-EXEC
021100081105     C                   enddo
021200081105     C*
021300081105     C/EXEC SQL
021400081105     C+ close C1
021500081105     C/END-EXEC
021600081105     C*
021700030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
021800030430     C                   exsr      wrivab                                       => scarico VAB
021900010202     C*
022000010202     C                   endif
022100990910
022200990910     C* Se non ci sono record con errori ...
022300000710     C                   if        §ctrno = 0
022400990910     C* ... restituisco esito OK.
022500990921     C                   eval      wrkesito = '0'
022600990910     C                   else
022700010201     C                   if        §ctrokvb > 0
022800990921     C                   eval      wrkesito = '1'
022900000710     C                   else
023000000710     C                   eval      wrkesito = '2'
023100990910     C                   endif
023200000710     C                   endif
023300990910     C*
023400990914     C                   if        %open(tivin00r)
023500990908     C                   close     tivin00r
023600990914     C                   endif
023700021017     C                   if        %open(fivabwwr)
023800021017     C                   close     fivabwwr
023900990914     C                   endif
024000021017     C                   if        %open(fivatwwr)
024100021017     C                   close     fivatwwr
024200010201     C                   endif
024300021017     C*
024400021017     C                   if        vlrpoi <> 999
024500021017     C                   eval      vlrpoi = vlrpoi
024600021017     C                   endif
024700990910     C*
024800010201     C                   if        §ctrokvb > 0
024900021017     C                             and vlrpoi > *zeros
025000010202     C                   exsr      invio
025100990920     C                   endif
025200990920     C*
025300910830     C                   ENDSR
025400000613     C***
025500990920
025600000801     C*----------------------------------------------------*
025700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025800000801     C*----------------------------------------------------*
025900010201     C     INZVAR        BEGSR
026000000801     C*
026100010201     C                   Z-ADD     *zeros        Num5_0
026200030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026300030310     C                   MOVEL     '0'           FlgCAS            1
026400000801     C*
026500000801     C                   ENDSR
026600051229     C*----------------------------------------------------*
026700051229     C*  REPERIMENTO NUMERATORE X NSP (VAB e VAT)
026800051229     C*----------------------------------------------------*
026900051229     C     REPNUM        BEGSR
027000051229     C*
027100051229     C* NSP => Stacco un numeratore da AZNUM
027200051229     C                   clear                   TRUL33DS
027300051229     C                   eval      I33OPE = *zeros
027400051229     C                   eval      I33CNU = 302
027500051229     C                   eval      I33NUM = 1
027600051229     C                   movel     TRUL33DS      KPJBU
027700051229     C                   call      'TRUL33R'
027800051229     C                   parm                    KPJBA
027900051229     C                   movel     KPJBU         TRUL33DS
028000051229     C                   if        O33ERR = *zeros
028100051229     C                   z-add     O33NRF        VABNSP
028200051229     C                   z-add     O33NRF        VATNSP
028300051229     C                   else
028400051229     C                   Z-ADD     1             errore
028500081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
028600051229     C                             + ' ' + 'VABNSP VATNSP'
028700051229     C                   endif
028800051229     C*
028900051229     C                   ENDSR
029000000801     C*----------------------------------------------------*
029100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029200000801     C*----------------------------------------------------*
029300000801     C     DEFCAM        BEGSR
029400000801     C*
029500020619     C* Imposto i valori di default...
029600041130     C                   Z-ADD     0062615       VABCCM
029700041130     C                   Z-ADD     0062615       VATCCM
029800021017     C                   Z-ADD     vlrpoi        VABFGS
029900021017     C                   Z-ADD     vlrpoi        VATFGS
030000041130     C                   Z-ADD     006           VABLNP
030100041130     C                   Z-ADD     006           VATLNP
030200030226     C                   Z-ADD     000           VABCTR
030300130409     C  N53              MOVEL     '7Q'          VABCTM
030400130409     C   53              MOVEL     '7T'          VABCTM
030500020619     C* ... e poi verifico se sono stati passati come parametri
030600020619     C                   IF        vlrppt > *blanks
030700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030800020619     C                   EXSR      CHKNUM
030900020619     C                   IF        PiInt=*on
031000020619     C                   Z-ADD     PiVal         VABCCM
031100020619     C                   Z-ADD     PiVal         VATCCM
031200020619     C                   ENDIF
031300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031400020619     C                   EXSR      CHKNUM
031500020619     C                   IF        PiInt=*on
031600020619     C                   Z-ADD     PiVal         VABLNP
031700020619     C                   Z-ADD     PiVal         VATLNP
031800020619     C                   ENDIF
031900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032000020619     C                   EXSR      CHKNUM
032100020619     C                   IF        PiInt=*on
032200020619     C                   Z-ADD     PiVal         VABCTR
032300020619     C                   ENDIF
032400120613     C                   EVAL      VABTSP=%subst(vlrppt:14:1)
032500130406     C*
032600020619     C                   ENDIF
032700000801     C*
032800000801     C                   ENDSR
032900000801     C*----------------------------------------------------*
033000030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033100000801     C*----------------------------------------------------*
033200010201     C     IMPVAB        BEGSR
033300000801     C*
033400020305     C                   EXSR      INZVAR
033500020305     C                   EXSR      DEFCAM
033600010305     C*
033700030226     C                   Z-ADD     *zeros        errore            1 0
033800000830     C                   MOVEL     datcor        VABAAS
033900000830     C                   MOVE      datcor        VABMGS
034000030226     C*
034100081027     C                   EVAL      VABRSD=%trim(%subst(vindta:797:35))
034200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
034400020117     C* ==
034500081027     C                   EVAL      VABRD2=%trim(%subst(vindta:832:35))
034600081027     C                   EVAL      VABIND=%trim(%subst(vindta:897:35))
034700081027     C                   EVAL      VABLOD=%trim(%subst(vindta:1009:35))
034800081027     C                   EVAL      VABPRD=%trim(%subst(vindta:1109:2))
034900130502     C                   EVAL      VABCAD=%trim(%subst(vindta:997:9))
035000150304     C                   EVAL      VABRMA=%trim(%subst(vindta:1221:3))
035100081027     C                   EVAL      VABNOT=%trim(%subst(vindta:1605:35))
035200081027     C                   EVAL      VABNT2=%trim(%subst(vindta:1640:35))
035300081027     C                   IF        %subst(vindta:1459:1) = 'E'
035400031201     C                   EVAL      VABTSP='E'
035500031201     C                   ENDIF
035600081027     C                   IF        %subst(vindta:1723:1) = '1'
035700081027     C                   EVAL      VABTC2='P'
035800081027     C                   ENDIF
035900081027     C                   IF        %subst(vindta:1724:1) = '1'
036000081027     C                   EVAL      VABTC1='A'
036100081027     C                   ENDIF
036200051229     C* RMN
036300110207     C                   EVAL      PiStr=%trim(%subst(vindta:1224:7))
036400010201     C                   EXSR      CHKNUM
036500010201     C                   IF        PiInt=*on
036600031201     C                   Z-ADD     PiVal         VABRMN
036700010201     C                   ELSE
036800010201     C                   ADD       1             errore
036900081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037000051229     C                             + ' ' + 'VABRMN'
037100010201     C                   ENDIF
037200081027     C* DCR
037300081027     C                   IF        %subst(vindta:1705:8) <> *blanks
037400081027     C                   EVAL      PiStr=%trim(%subst(vindta:1709:4) +
037500081027     C                                         %subst(vindta:1707:2) +
037600081027     C                                         %subst(vindta:1705:2))
037700081027     C                   EXSR      CHKNUM
037800081027     C                   IF        PiInt=*on
037900081027     C                   Z-ADD     PiVal         VABDCR
038000081027     C                   ELSE
038100081027     C                   ADD       1             errore
038200081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038300081027     C                             + ' ' + 'VABDCR'
038400081027     C                   ENDIF
038500081027     C                   ENDIF
038600081027     C* NCL
038700081027     C                   EVAL      VABNCL = VABNCL + 1
038800030226     C* PKB
038900081027     C                   EVAL      PiStr=%trim(%subst(vindta:1537:9))
039000030107     C                   EXSR      CHKNUM
039100030107     C                   IF        PiNum=*on
039200090702     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 decim.
039300090702     C                   ADD       PiVal         VABPKB
039400030107     C                   ELSE
039500030107     C                   ADD       1             errore
039600081105     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039700030107     C                             + ' ' + 'VABPKB'
039800030107     C                   ENDIF
039900090320     C* VLB
040000090320     C                   EVAL      PiStr=%trim(%subst(vindta:1546:9))
040100090320     C                   EXSR      CHKNUM
040200090320     C                   IF        PiNum=*on
040300090702     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 decim.
040400090702     C                   ADD       PiVal         VABVLB
040500090320     C                   ELSE
040600090320     C                   ADD       1             errore
040700090320     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800090320     C                             + ' ' + 'VABVLB'
040900090320     C                   ENDIF
041000030310     C*
041100030310     C* Considerazioni finali su CBO/CAS
041200030310     C                   IF        FlgCAS = '1'
041300010205     C                   EVAL      VABCBO = '4'
041400010205     C                   ELSE
041500010205     C                   EVAL      VABCBO = '1'
041600010205     C                   ENDIF
041700020305     C*
041800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
041900011113     C                   EXSR      CHKIMPDIV
042000010202     C*
042100000801     C* Ebbene...
042200000801     C                   ADD       1             §CTRMO
042300010201     C                   IF        errore <> *zeros
042400000801     C                   ADD       1             §CTRNO
042500081105     C                   EVAL      x_vinflg = '2'
042600000801     C                   ELSE
042700010201     C                   ADD       1             §CTROKVB
042800000801     C                   ENDIF
042900000801     C*
043000000801     C                   ENDSR
043100030226     C*----------------------------------------------------*
043200030226     C*  SCARICAMENTO BUFFER RECORDS VAB
043300030226     C*----------------------------------------------------*
043400030226     C     WRIVAB        BEGSR
043500030310     C*
043600030310     C                   write     FIVAB000
043700030226     C*
043800030226     C                   ENDSR
043900030226     C*----------------------------------------------------*
044000030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
044100030226     C*----------------------------------------------------*
044200031201     C     EXEVAT        BEGSR
044300030226     C*
044400030226     C                   EXSR      INZVAR
044500030226     C                   EXSR      DEFCAM
044600030226     C*
044700030226     C                   MOVEL     datcor        VATAAS
044800030226     C*
044900030415     C* Valorizzo il barcode
045000031201     C                   EVAL      VATTRC='E'
045100081027     C                   EVAL      VATNOT=%trim(%subst(vindta:1490:30))
045200031201     C                   exsr      wrivat                                       => scarico VAT
045300130409     C*
045400130409     C* Solo a rottura di spedizione
045500130409     C                   if        savNSP <> vatNSP
045600130409     C                   eval      savNSP =  vatNSP
045700031201     C*
045800031201     C* Valorizzo il telefono destinatario (se valorizzato)
045900031201     C                   EVAL      VATTRC='B'
046000081027     C                   EVAL      VATNOT=%trim(%subst(vindta:1132:24))
046100081027     C                   exsr      wrivat                                       => scarico VAT
046200120514     C*
046300140707     C* Valorizzo l'email destinatario (se valorizzato e se NON negato)
046400140707     C                   IF        not *in54
046500120514     C                   IF        %subst(vindta:1156:50) <> *blanks
046600120514     C                   EVAL      VATTRC='I'
046700120514     C                   EVAL      VATNOT=%trim(%subst(vindta:1156:35))
046800120514     C                   exsr      wrivat                                       => scarico VAT
046900120514     C                   EVAL      VATTRC='J'
047000120514     C                   EVAL      VATNOT=%trim(%subst(vindta:1156+35:50-35))
047100120514     C                   exsr      wrivat                                       => scarico VAT
047200120514     C                   ENDIF
047300140707     C                   ENDIF
047400130409     C*
047500130409     C* Considerazioni tipo record 'P' per PDF
047600130409     C*
047700130409     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
047800130409     C                   if        *in53
047900130409     C*
048000130409     C                   eval      pIn_MaskPDF =
048100130409     C                                 %subst(vindta:1:3)+'_*'+
048200130409     C                                 %subst(vindta:1217:4)+'_'+
048300130409     C                                 %trim(%subst(vindta:1221:3))+'_'+
048400130409     C                                 %trim(%subst(vindta:1224:7))+'_'+
048500130409     C                                 %trim(%subst(vindta:1211:6))+'*.pdf'
048600130409     C*
048700130409     C* Verifica codice identificativo azione da utilizzare
048800130409     C                   eval      pIn_CdIdAz = '10'
048900130409     C                   if        %subst(vindta:1156:50) <> *blanks
049000130409     C                   eval      pIn_CdIdAz = '11'
049100130409     C                   endif
049200130409     C*
049300130409     C                   call      'TIS7P2SR'
049400130409     C                   parm      'W'           pIn_Opz           1
049500130409     C                   parm                    tivlrds
049600130409     C                   parm                    FIVABDS
049700130409     C                   parm                    pIn_CdIdAz        2
049800130409     C                   parm                    pIn_MaskPDF      50
049900130409     C                   parm      *blanks       pOut_Esito        1
050000130409     C*
050100130409     C                   endif
050200130409     C*
050300130409     C                   endif
050400030226     C*
050500030226     C                   ENDSR
050600010201     C*----------------------------------------------------*
050700021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
050800010201     C*----------------------------------------------------*
050900020305     C     WRIVAT        BEGSR
051000010201     C*
051100081027     C                   if        vatNOT <> *blanks
051200021017     C                   write     FIVAT000
051300081027     C                   endif
051400010201     C*
051500010201     C                   ENDSR
051600010202     C*----------------------------------------------------*
051700021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051800010202     C*----------------------------------------------------*
051900020305     C     PREVAT        BEGSR
052000010202     C*
052100021017     C* Compongo il nome del membro da dare al FIVATWWR
052200010202     C                   eval      parmbr = vlrhdl
052300010202     C                   movel     'M'           parmbr
052400050317     C                   eval      parccm = '00062615'
052500010202     C                   eval      paropz = '1'
052600010202     C* Effettuo la chiamata al CLLE preposto
052700031201     C                   call(e)   'TITVVTC'
052800010202     C                   parm                    parccm
052900010202     C                   parm                    parmbr
053000010202     C                   parm                    paropz
053100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053200010202     C                   if        %error
053300010202     C                   movel     '1'           chkcall
053400010202     C                   else
053500010202     C                   movel     '0'           chkcall
053600010202     C                   endif
053700010202     C*
053800010202     C                   ENDSR
053900000801     C*----------------------------------------------------*
054000000801     C*  CONTROLLO NUMERICITA' CAMPI
054100000801     C*----------------------------------------------------*
054200000801     C     CHKNUM        BEGSR
054300000801     C*
054400081027     C                   IF        PiDecChr = *blanks
054500081027     C                   EVAL      PiDecChr = ','
054600081027     C                   ENDIF
054700081027     C*
054800081027     C                   callp     UBISNUM_Check(PiStr
054900081027     C                                          :PiDecChr
055000081027     C                                          :PiVal
055100081027     C                                          :PiNum
055200081027     C                                          :PiInt)
055300000801     C*
055400000801     C                   ENDSR
055500000801     C***
055600000801
055700011113
055800011113     C*----------------------------------------------------*
055900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
056000011113     C*----------------------------------------------------*
056100011113     C     CHKIMPDIV     BEGSR
056200011113     C*
056300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
056400011113     C                   Z-ADD     *zeros        wrkDec            9 9
056500011113     C*
056600011113     C* Come prima cosa effettuo considerazioni sulla divisa
056700011113     C                   IF        vabIAS > *zeros
056800011113     C                   IF        vabVAS <> 'EUR'
056900011113     C                   EVAL      vabVAS =  'ITL'
057000011113     C                   ENDIF
057100011113     C                   ENDIF
057200011113     C*
057300011113     C                   IF        vabCAS > *zeros
057400011113     C                   IF        vabVCA <> 'EUR'
057500011113     C                   EVAL      vabVCA =  'ITL'
057600011113     C                   ENDIF
057700011113     C                   ENDIF
057800011113     C*
057900011113     C                   IF        vabVMD > *zeros
058000020305     C                   IF        vabVAD <> 'EUR'
058100011113     C                   EVAL      vabVAD =  'ITL'
058200011113     C                   ENDIF
058300011113     C                   ENDIF
058400011113     C*
058500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
058600011113     C                   Z-ADD     vabIAS        wrkDec
058700011113     C                   IF        wrkDec > *zeros
058800011113     C                   IF        vabVAS = 'ITL'
058900011113     C                   EVAL      vabIAS = *zeros
059000011113     C                   ENDIF
059100011113     C                   ENDIF
059200011113     C*
059300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
059400011113     C                   Z-ADD     vabCAS        wrkDec
059500011113     C                   IF        wrkDec > *zeros
059600011113     C                   IF        vabVCA = 'ITL'
059700011113     C                   EVAL      vabCAS = *zeros
059800011113     C                   ENDIF
059900011113     C                   ENDIF
060000011113     C*
060100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
060200011113     C                   Z-ADD     vabVMD        wrkDec
060300011113     C                   IF        wrkDec > *zeros
060400011113     C                   IF        vabVAD = 'ITL'
060500011113     C                   EVAL      vabVMD = *zeros
060600011113     C                   ENDIF
060700011113     C                   ENDIF
060800011113     C*
060900011113     C                   ENDSR
061000011113     C***
061100021017
061200000801
061300000801
061400990920      /TITLE Invio dei dati al punto operativo.
061500010202     C     invio         BEGSR
061600990920     C*
061700021017     C* 1° invio FIVAT
061800010201     C                   reset                   dscmz
061900021017     C                   move      vlrpoi        cmzdst
062000021017     C                   eval      cmzfld = 'FIVATWWR'
062100010201     C                   eval      cmzmbd = vlrhdl
062200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300021009     C***                if        prmfir = *blanks
062400021017     C                   eval      cmzfla = 'FIVAT00F'
062500021017     C                   eval      cmzmba = 'FIVAT00F'
062600021009     C***                else
062700021009     C***                eval      cmzfla = prmfir
062800021009     C***                eval      cmzmba = prmfir
062900021009     C***                endif
063000010201     C                   eval      cmznrr = *zeros
063100020305     C                   move      §ctrokvt      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010205     C                   if        %error
063700010205     C                             or cmzerr = '1'
063800010205     C                             or esito  = '1'
063900010205     C                   eval      wrkesito = '3'
064000010205     C                   else
064100010201     C*
064200021017     C* 2° invio FIVAB
064300010201     C                   reset                   dscmz
064400021017     C                   move      vlrpoi        cmzdst
064500010201     C                   eval      cmzfld = vlrfou
064600010201     C                   eval      cmzmbd = vlrhdl
064700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
064800021009     C***                if        prmfir = *blanks
064900021017     C                   eval      cmzfla = 'FIVAB00F'
065000021017     C                   eval      cmzmba = 'FIVAB00F'
065100021009     C***                else
065200021009     C***                eval      cmzfla = prmfir
065300021009     C***                eval      cmzmba = prmfir
065400021009     C***                endif
065500010201     C                   eval      cmznrr = *zeros
065600010201     C                   move      §ctrokvb      cmznrr
065700021018     C                   eval      cmzlba = vlrfl1
065800010201     C                   call(e)   'TIS711C'
065900010201     C                   parm                    dscmz
066000010201     C                   parm      *blanks       esito
066100010201     C                   if        %error
066200010201     C                             or cmzerr = '1'
066300010201     C                             or esito  = '1'
066400010201     C                   eval      wrkesito = '3'
066500010201     C                   endif
066600010205     C                   endif
066700990920     C*
066800000613     C                   ENDSR
066900000613     C***
067000070411
067100090702     C***  *pssr         BEGSR
067200070411     C*
067300090702     C*                  if        %open(tivin00r)
067400090702     C*                  close     tivin00r
067500090702     C*                  endif
067600090702     C*                  if        %open(fivabwwr)
067700090702     C*                  close     fivabwwr
067800090702     C*                  endif
067900090702     C*                  if        %open(fivatwwr)
068000090702     C*                  close     fivatwwr
068100090702     C*                  endif
068200070411     C*
068300070411     C* Effettuo la chiamata al CLLE preposto
068400090702     C*                  call(e)   'TITVVTC'
068500090702     C*                  parm                    parccm
068600090702     C*                  parm                    parmbr
068700090702     C*                  parm      '2'           paropz
068800090702     C*
068900090702     C*                  eval      wrkesito = '2'
069000090702     C*
069100090702     C*                  seton                                        LR
069200070411     C*
069300090702     C*                  ENDSR     '*CANCL'
069400070411     C***
069500070411
069600990910
069700000613     C     *inzsr        BEGSR
069800990910     C*
069900990910     C     *entry        plist
070000990920     C                   parm                    tivlrds
070100990921     C                   parm      wrkesito      esito
070200000724     C                   parm                    prmlit
070300000710     C                   parm                    prmfir
070400130406     C*
070500130406     C* Verifico se sui parametri del traduttore è richiesto la scrittura forzata del
070600130406     C* tipo record 'P' del VAT (packing list PDF)
070700130406     C                   if        %subst(vlrppt:15:1) = 'S'
070800130406     C                   seton                                        53
070900130406     C                   else
071000130406     C                   setoff                                       53
071100130406     C                   endif
071200140707     C*
071300140707     C* Verifico se sui parametri del traduttore è richiesto il NON invio alert
071400140707     C* al destinatario
071500140707     C                   if        %subst(vlrppt:16:1) = 'N'
071600140707     C                   seton                                        54
071700140707     C                   else
071800140707     C                   setoff                                       54
071900140707     C                   endif
072000000613     C*
072100000830     C* CALCOLA LA DATA CORRENTE
072200130406     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072300130406     C                   eval      datcor = %dec(%date() : *ISO)
072400000830     C*
072500000613     C                   ENDSR
072600000613     C***
