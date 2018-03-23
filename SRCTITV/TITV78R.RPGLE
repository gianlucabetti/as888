000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800031201     Ftitv78p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100031201     Ftitv78ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500030415     D depspe          s              7    INZ(*blanks)
003600030415     D curspe          s              7    INZ(*blanks)
003700030415     D cntspe          s              7  0 INZ(*zeros)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200010213
004300000801     D  Num5_0         s              5  0
004400030107     D  Num7_0         s              7  0
004500000830
004600000830     D*------------------
004700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000830     D*------------------
004900000830     D WLBDA8          DS                  INZ
005000000830     D  G08DAT                 1      8  0
005100000830     D  G08INV                 9     16  0
005200000830     D  G08ERR                17     17
005300000830     D  G08TGI                18     22  0
005400051229     D*------------------
005500051229     D* DS REPERIMENTO NUMERATORE
005600051229     D*------------------
005700051229     D trul33ds      e ds                  inz
005800051229     D*------------------
005900051229     D* DS ARCHITETTURA
006000051229     D*------------------
006100051229     D kpjba         e ds                  inz
006200051229     D*------------------
006300990908
006400010201
006500010201
006600990915     C                   time                    wrkdata
006700990915     C                   time                    wrkora
006800000913     C                   reset                   rrnum
006900990921     C                   reset                   esito
007000990921     C                   reset                   wrkesito
007100000724     C*
007200000724     C* SE OCCORRE SPEDIRE IN FILIALE
007300021017     C                   if        vlrpoi <> *zeros
007400000724     C*
007500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700021017     C     vlrpoi        chain     azorg01l
007800000724     C                   if        %found
007900000616     C                   movel(p)  CMD(1)        depcmd
008000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008100000616     C*
008200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000616     C                   PARM                    depcmd
008600000616     C                   PARM                    LENGH
008700000724     C*
008800000724     C                   endif
008900000724     C                   endif
009000000616     C*
009100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200000616     C                   movel(p)  CMD(2)        depcmd
009300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009400000616     C*
009500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009600011113     C                   Z-ADD     150           LENGH            15 5
009700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009800000616     C                   PARM                    depcmd
009900000616     C                   PARM                    LENGH
010000000616     C*
010100031201     C                   if        not %open(titv78ps)
010200031201     C                   open      titv78ps
010300000616     C                   except    testdett
010400000613     C                   endif
010500000613     C*
010600030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010700000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010800000613     C*
010900010202     C* Effettuo la chiamata al CLLE preposto
011000031201     C                   call(e)   'TITVVTC'
011100010202     C                   parm                    parccm
011200010202     C                   parm                    parmbr
011300010202     C                   parm      '2'           paropz
011400010202     C*
011500031201     C                   if        %open(titv78ps)
011600000616     C                   except    findett
011700031201     C                   close     titv78ps
011800000613     C                   endif
011900000616     C*
012000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    CMD(3)
012400000616     C                   PARM                    LENGH
012500000616     C*
012600010201     C                   seton                                        LR
012700000613
012800000613
012900000613     C*--------------------------------------------------------
013000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013100000613     C*--------------------------------------------------------
013200000621     C     STPR          BEGSR
013300000613     C*
013400031201     C                   if        not %open(titv78p)
013500031201     C                   open      titv78p
013600990915     C                   endif
013700990915     C*
013800990915     C                   except    riepilogo
013900990915     C*
014000031201     C                   if        %open(titv78p)
014100031201     C                   close     titv78p
014200990914     C                   endif
014300990910     C*
014400000613     C                   ENDSR
014500000613     C***
014600990908
014700000801
014800910830     C*--------------------------------------------------------
014900030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
015000910830     C*--------------------------------------------------------
015100030226     C     RWFILE        BEGSR
015200990910     C*
015300990914     C                   if        not %open(tivin00r)
015400990908     C                   open      tivin00r
015500990914     C                   endif
015600021017     C                   if        not %open(fivabwwr)
015700021017     C                   open      fivabwwr
015800990914     C                   endif
015900021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016000020305     C                   exsr      prevat
016100010201     C*
016200010202     C                   if        chkcall = '0'
016300010202     C*
016400021017     C                   if        not %open(fivatwwr)
016500021017     C                   open      fivatwwr
016600010201     C                   endif
016700990910     C*
016800010201     C                   clear                   §CTROKVB          5 0
016900020305     C                   clear                   §CTROKVT          5 0
017000000801     C                   clear                   §CTRMO            5 0
017100000801     C                   clear                   §CTRNO            5 0
017200990910     C*
017300921023     C                   DO        *HIVAL
017400990913     C*
017500990915     C                   READ      tivin00r                               70
017600000905     C                   if        vindta > *blanks
017700000613     C                   add       1             rrnum
017800000801     C*
017900000801     C                   if        *in70 = *off
018000000801     C                             and
018100000801     C                             (vinflg = *blanks
018200000801     C                              or vinflg = '0'
018300000801     C                              or vinflg = '2')
018400000801     C*
018500000801     C                   clear                   vinmsg
018600030310     C                   eval      vinflg = '1'
018700030310     C*
018800030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018900031201     C                   eval      PiStr=%trim(%subst(vindta:397:7))
019000030411     C                   movel(p)  PiStr         curspe
019100030411     C                   add       1             cntspe
019200030411     C*
019300030411     C                   if        cntspe = 1
019400030411     C* A rottura inizializzo campi di totalizzazione
019500030411     C                   clear                   fivab000
019600051229     C                   exsr      repnum                                       => calcolo NSP
019700051229     C*
019800030411     C                   exsr      impvab                                       => carico VAB
019900031201     C                   exsr      exevat                                       => carico VAT
020000030411     C                   eval      depspe = curspe
020100030411     C                   else
020200030411     C                   if        curspe <> depspe
020300030411     C                   exsr      wrivab                                       => scarico VAB
020400030411     C* A rottura inizializzo campi di totalizzazione
020500030411     C                   clear                   fivab000
020600051229     C                   exsr      repnum                                       => calcolo NSP
020700051229     C*
020800030411     C                   exsr      impvab                                       => carico VAB
020900031201     C                   exsr      exevat                                       => carico VAT
021000030411     C                   eval      depspe = curspe
021100030411     C                   else
021200030411     C                   exsr      impvab                                       => carico VAB
021300031201     C                   exsr      exevat                                       => carico VAT
021400030411     C                   endif
021500030411     C                   endif
021600000905     C*
021700000905     C                   else
021800000905     C                   eval      vinflg = '1'
021900000905     C                   endif
022000030411     C                   endif
022100000905     C*
022200000905     C  N70              update    tivin000
022300000905     C*
022400991022     C  N70              ENDdo
022500030430     C*
022600030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
022700030430     C                   exsr      wrivab                                       => scarico VAB
022800010202     C*
022900010202     C                   endif
023000990910
023100990910     C* Se non ci sono record con errori ...
023200000710     C                   if        §ctrno = 0
023300990910     C* ... restituisco esito OK.
023400990921     C                   eval      wrkesito = '0'
023500990910     C                   else
023600010201     C                   if        §ctrokvb > 0
023700990921     C                   eval      wrkesito = '1'
023800000710     C                   else
023900000710     C                   eval      wrkesito = '2'
024000990910     C                   endif
024100000710     C                   endif
024200990910     C*
024300990914     C                   if        %open(tivin00r)
024400990908     C                   close     tivin00r
024500990914     C                   endif
024600021017     C                   if        %open(fivabwwr)
024700021017     C                   close     fivabwwr
024800990914     C                   endif
024900021017     C                   if        %open(fivatwwr)
025000021017     C                   close     fivatwwr
025100010201     C                   endif
025200021017     C*
025300021017     C                   if        vlrpoi <> 999
025400021017     C                   eval      vlrpoi = vlrpoi
025500021017     C                   endif
025600990910     C*
025700010201     C                   if        §ctrokvb > 0
025800021017     C                             and vlrpoi > *zeros
025900010202     C                   exsr      invio
026000990920     C                   endif
026100990920     C*
026200910830     C                   ENDSR
026300000613     C***
026400990920
026500000801     C*----------------------------------------------------*
026600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026700000801     C*----------------------------------------------------*
026800010201     C     INZVAR        BEGSR
026900000801     C*
027000010201     C                   Z-ADD     *zeros        Num5_0
027100030107     C                   Z-ADD     *zeros        Num7_0
027200030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027300030310     C                   MOVEL     '0'           FlgCAS            1
027400000801     C*
027500000801     C                   ENDSR
027600051229
027700051229     C*----------------------------------------------------*
027800051229     C*  REPERIMENTO NUMERATORE X NSP (VAB e VAT)
027900051229     C*----------------------------------------------------*
028000051229     C     REPNUM        BEGSR
028100051229     C*
028200051229     C* NSP => Stacco un numeratore da AZNUM
028300051229     C                   clear                   TRUL33DS
028400051229     C                   eval      I33OPE = *zeros
028500051229     C                   eval      I33CNU = 302
028600051229     C                   eval      I33NUM = 1
028700051229     C                   movel     TRUL33DS      KPJBU
028800051229     C                   call      'TRUL33R'
028900051229     C                   parm                    KPJBA
029000051229     C                   movel     KPJBU         TRUL33DS
029100051229     C                   if        O33ERR = *zeros
029200051229     C                   z-add     O33NRF        VABNSP
029300051229     C                   z-add     O33NRF        VATNSP
029400051229     C                   else
029500051229     C                   Z-ADD     1             errore
029600051229     C                   EVAL      vinmsg = %trimr(vinmsg)
029700051229     C                             + ' ' + 'VABNSP VATNSP'
029800051229     C                   endif
029900051229     C*
030000051229     C                   ENDSR
030100000801     C*----------------------------------------------------*
030200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030300000801     C*----------------------------------------------------*
030400000801     C     DEFCAM        BEGSR
030500000801     C*
030600020619     C* Imposto i valori di default...
030700031201     C                   Z-ADD     0610329       VABCCM
030800031201     C                   Z-ADD     0610329       VATCCM
030900021017     C                   Z-ADD     vlrpoi        VABFGS
031000021017     C                   Z-ADD     vlrpoi        VATFGS
031100031201     C                   Z-ADD     061           VABLNP
031200031201     C                   Z-ADD     061           VATLNP
031300030226     C                   Z-ADD     000           VABCTR
031400021017     C                   MOVEL     '7Q'          VABCTM
031500020619     C* ... e poi verifico se sono stati passati come parametri
031600020619     C                   IF        vlrppt > *blanks
031700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031800020619     C                   EXSR      CHKNUM
031900020619     C                   IF        PiInt=*on
032000020619     C                   Z-ADD     PiVal         VABCCM
032100020619     C                   Z-ADD     PiVal         VATCCM
032200020619     C                   ENDIF
032300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032400020619     C                   EXSR      CHKNUM
032500020619     C                   IF        PiInt=*on
032600020619     C                   Z-ADD     PiVal         VABLNP
032700020619     C                   Z-ADD     PiVal         VATLNP
032800020619     C                   ENDIF
032900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033000020619     C                   EXSR      CHKNUM
033100020619     C                   IF        PiInt=*on
033200020619     C                   Z-ADD     PiVal         VABCTR
033300020619     C                   ENDIF
033400020619     C                   ENDIF
033500000801     C*
033600000801     C                   ENDSR
033700000801     C*----------------------------------------------------*
033800030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033900000801     C*----------------------------------------------------*
034000010201     C     IMPVAB        BEGSR
034100000801     C*
034200020305     C                   EXSR      INZVAR
034300020305     C                   EXSR      DEFCAM
034400010305     C*
034500030226     C                   Z-ADD     *zeros        errore            1 0
034600000830     C                   MOVEL     datcor        VABAAS
034700000830     C                   MOVE      datcor        VABMGS
034800030226     C*
034900031201     C                   EVAL      VABRSD=%trim(%subst(vindta:250:30))
035000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
035200020117     C* ==
035300031201     C                   EVAL      VABIND=%trim(%subst(vindta:280:30))
035400031201     C                   EVAL      VABLOD=%trim(%subst(vindta:322:30))
035500031201     C                   EVAL      VABPRD=%trim(%subst(vindta:352:2))
035600040324     C                   EVAL      VABRMA=%trim(%subst(vindta:640:12))
035700050420     C                   EVAL      VABNT2=%trim(%subst(vindta:602:30))
035800031201     C                   IF        %subst(vindta:490:1) = 'E'
035900031201     C                   EVAL      VABTSP='E'
036000031201     C                   ENDIF
036100030226     C* CAP
036200031201     C                   EVAL      PiStr=%trim(%subst(vindta:310:12))
036300010201     C                   EXSR      CHKNUM
036400010201     C                   IF        PiInt=*on
036500010201     C                   Z-ADD     PiVal         Num5_0
036600021017     C                   MOVEL     Num5_0        VABCAD
036700010201     C                   ELSE
036800010201     C                   ADD       1             errore
036900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037000021017     C                             + ' ' + 'VABCAD'
037100010201     C                   ENDIF
037200051229     C* RMN
037300031201     C                   EVAL      PiStr=%trim(%subst(vindta:397:7))
037400010201     C                   EXSR      CHKNUM
037500010201     C                   IF        PiInt=*on
037600030107     C                   Z-ADD     PiVal         Num7_0
037700031201     C                   Z-ADD     PiVal         VABRMN
037800010201     C                   ELSE
037900010201     C                   ADD       1             errore
038000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038100051229     C                             + ' ' + 'VABRMN'
038200010201     C                   ENDIF
038300030226     C* NCL
038400030415     C                   EVAL      VABNCL = VABNCL + 1
038500030226     C* PKB
038600031201     C                   EVAL      PiStr=%trim(%subst(vindta:534:9))
038700030107     C                   EXSR      CHKNUM
038800030107     C                   IF        PiNum=*on
038900031201     C                   EVAL      VABPKB=VABPKB+(PiVal/100)                    * gestisco 2 decim.
039000030107     C                   ELSE
039100030107     C                   ADD       1             errore
039200030107     C                   EVAL      vinmsg = %trimr(vinmsg)
039300030107     C                             + ' ' + 'VABPKB'
039400030107     C                   ENDIF
039500090320     C* VLB
039600090320     C                   EVAL      PiStr=%trim(%subst(vindta:543:9))
039700090320     C                   EXSR      CHKNUM
039800090320     C                   IF        PiNum=*on
039900090320     C                   EVAL      VABVLB=VABVLB+(PiVal/1000)                   * gestisco 3 decim.
040000090320     C                   ELSE
040100090320     C                   ADD       1             errore
040200090320     C                   EVAL      vinmsg = %trimr(vinmsg)
040300090320     C                             + ' ' + 'VABVLB'
040400090320     C                   ENDIF
040500040324     C* DCR
040600040324     C                   EVAL      PiStr=%trim(%subst(vindta:636:4) +
040700040324     C                                         %subst(vindta:634:2) +
040800040324     C                                         %subst(vindta:632:2))
040900040324     C                   EXSR      CHKNUM
041000040324     C                   IF        PiInt=*on
041100040324     C                   Z-ADD     PiVal         VABDCR
041200040324     C                   ELSE
041300040324     C                   SETON                                        32
041400040324     C                   Z-ADD     *zeros        VABDCR
041500040324     C                   EVAL      vinmsg = %trimr(vinmsg)
041600040324     C                             + ' ' + 'VABDCR'
041700040324     C                   ENDIF
041800030310     C*
041900030310     C* Considerazioni finali su CBO/CAS
042000030310     C                   IF        FlgCAS = '1'
042100010205     C                   EVAL      VABCBO = '4'
042200010205     C                   ELSE
042300010205     C                   EVAL      VABCBO = '1'
042400010205     C                   ENDIF
042500020305     C*
042600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042700011113     C                   EXSR      CHKIMPDIV
042800010202     C*
042900000801     C* Ebbene...
043000000801     C                   ADD       1             §CTRMO
043100010201     C                   IF        errore <> *zeros
043200000801     C                   ADD       1             §CTRNO
043300000801     C                   EVAL      recko = vindta
043400000801     C                   EXCEPT    rigadett
043500000801     C                   EVAL      vinflg = '2'
043600000801     C                   ELSE
043700010201     C                   ADD       1             §CTROKVB
043800000801     C                   ENDIF
043900000801     C*
044000000801     C                   ENDSR
044100030226     C*----------------------------------------------------*
044200030226     C*  SCARICAMENTO BUFFER RECORDS VAB
044300030226     C*----------------------------------------------------*
044400030226     C     WRIVAB        BEGSR
044500030310     C*
044600030310     C                   write     FIVAB000
044700030226     C*
044800030226     C                   ENDSR
044900030226     C*----------------------------------------------------*
045000030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
045100030226     C*----------------------------------------------------*
045200031201     C     EXEVAT        BEGSR
045300030226     C*
045400030226     C                   EXSR      INZVAR
045500030226     C                   EXSR      DEFCAM
045600030226     C*
045700030226     C                   MOVEL     datcor        VATAAS
045800030226     C*
045900030415     C* Valorizzo il barcode
046000031201     C                   EVAL      VATTRC='E'
046100070629     C                   IF        %subst(vindta:501:1) = '0'
046200070629     C                   EVAL      VATNOT=%trim(%subst(vindta:507:10))
046300070629     C                   ELSE
046400070629     C                   EVAL      VATNOT=%trim(%subst(vindta:501:16))
046500070629     C                   ENDIF
046600031201     C                   exsr      wrivat                                       => scarico VAT
046700031201     C*
046800031201     C* Valorizzo il telefono destinatario (se valorizzato)
046900031201     C                   IF        %subst(vindta:363:20) <> *blanks
047000031201     C                   EVAL      VATTRC='B'
047100031201     C                   EVAL      VATNOT=%trim(%subst(vindta:363:20))
047200031201     C                   exsr      wrivat                                       => scarico VAT
047300031201     C                   ENDIF
047400030226     C*
047500030226     C                   ENDSR
047600010201     C*----------------------------------------------------*
047700021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
047800010201     C*----------------------------------------------------*
047900020305     C     WRIVAT        BEGSR
048000010201     C*
048100021017     C                   write     FIVAT000
048200010201     C*
048300010201     C                   ENDSR
048400010202     C*----------------------------------------------------*
048500021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048600010202     C*----------------------------------------------------*
048700020305     C     PREVAT        BEGSR
048800010202     C*
048900021017     C* Compongo il nome del membro da dare al FIVATWWR
049000010202     C                   eval      parmbr = vlrhdl
049100010202     C                   movel     'M'           parmbr
049200031201     C                   eval      parccm = '00610329'
049300010202     C                   eval      paropz = '1'
049400010202     C* Effettuo la chiamata al CLLE preposto
049500031201     C                   call(e)   'TITVVTC'
049600010202     C                   parm                    parccm
049700010202     C                   parm                    parmbr
049800010202     C                   parm                    paropz
049900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050000010202     C                   if        %error
050100010202     C                   movel     '1'           chkcall
050200010202     C                   else
050300010202     C                   movel     '0'           chkcall
050400010202     C                   endif
050500010202     C*
050600010202     C                   ENDSR
050700000801     C*----------------------------------------------------*
050800000801     C*  CONTROLLO NUMERICITA' CAMPI
050900000801     C*----------------------------------------------------*
051000000801     C     CHKNUM        BEGSR
051100000801     C*
051200000801     C                   call(e)   'ISNUMERIC'
051300000801     C                   PARM                    PiStr            30
051400030107     C                   PARM      ','           PiDecChr          1
051500000801     C                   PARM      *ZEROS        PiVal            30 9
051600000801     C                   PARM      '0'           PiInt             1
051700000801     C                   PARM      '0'           PiNum             1
051800000801     C                   IF        %error
051900000801     C                   EVAL      PiInt=*off
052000030107     C                   EVAL      PiNum=*off
052100000801     C                   ENDIF
052200000801     C*
052300000801     C                   ENDSR
052400000801     C***
052500000801
052600011113
052700011113     C*----------------------------------------------------*
052800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052900011113     C*----------------------------------------------------*
053000011113     C     CHKIMPDIV     BEGSR
053100011113     C*
053200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053300011113     C                   Z-ADD     *zeros        wrkDec            9 9
053400011113     C*
053500011113     C* Come prima cosa effettuo considerazioni sulla divisa
053600011113     C                   IF        vabIAS > *zeros
053700011113     C                   IF        vabVAS <> 'EUR'
053800011113     C                   EVAL      vabVAS =  'ITL'
053900011113     C                   ENDIF
054000011113     C                   ENDIF
054100011113     C*
054200011113     C                   IF        vabCAS > *zeros
054300011113     C                   IF        vabVCA <> 'EUR'
054400011113     C                   EVAL      vabVCA =  'ITL'
054500011113     C                   ENDIF
054600011113     C                   ENDIF
054700011113     C*
054800011113     C                   IF        vabVMD > *zeros
054900020305     C                   IF        vabVAD <> 'EUR'
055000011113     C                   EVAL      vabVAD =  'ITL'
055100011113     C                   ENDIF
055200011113     C                   ENDIF
055300011113     C*
055400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055500011113     C                   Z-ADD     vabIAS        wrkDec
055600011113     C                   IF        wrkDec > *zeros
055700011113     C                   IF        vabVAS = 'ITL'
055800011113     C                   EVAL      vabIAS = *zeros
055900011113     C                   ENDIF
056000011113     C                   ENDIF
056100011113     C*
056200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056300011113     C                   Z-ADD     vabCAS        wrkDec
056400011113     C                   IF        wrkDec > *zeros
056500011113     C                   IF        vabVCA = 'ITL'
056600011113     C                   EVAL      vabCAS = *zeros
056700011113     C                   ENDIF
056800011113     C                   ENDIF
056900011113     C*
057000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057100011113     C                   Z-ADD     vabVMD        wrkDec
057200011113     C                   IF        wrkDec > *zeros
057300011113     C                   IF        vabVAD = 'ITL'
057400011113     C                   EVAL      vabVMD = *zeros
057500011113     C                   ENDIF
057600011113     C                   ENDIF
057700011113     C*
057800011113     C                   ENDSR
057900011113     C***
058000021017
058100000801
058200000801
058300990920      /TITLE Invio dei dati al punto operativo.
058400010202     C     invio         BEGSR
058500990920     C*
058600021017     C* 1° invio FIVAT
058700010201     C                   reset                   dscmz
058800021017     C                   move      vlrpoi        cmzdst
058900021017     C                   eval      cmzfld = 'FIVATWWR'
059000010201     C                   eval      cmzmbd = vlrhdl
059100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059200021009     C***                if        prmfir = *blanks
059300021017     C                   eval      cmzfla = 'FIVAT00F'
059400021017     C                   eval      cmzmba = 'FIVAT00F'
059500021009     C***                else
059600021009     C***                eval      cmzfla = prmfir
059700021009     C***                eval      cmzmba = prmfir
059800021009     C***                endif
059900010201     C                   eval      cmznrr = *zeros
060000020305     C                   move      §ctrokvt      cmznrr
060100021018     C                   eval      cmzlba = vlrfl1
060200010201     C                   call(e)   'TIS711C'
060300010201     C                   parm                    dscmz
060400010201     C                   parm      *blanks       esito
060500010205     C                   if        %error
060600010205     C                             or cmzerr = '1'
060700010205     C                             or esito  = '1'
060800010205     C                   eval      wrkesito = '3'
060900010205     C                   else
061000010201     C*
061100021017     C* 2° invio FIVAB
061200010201     C                   reset                   dscmz
061300021017     C                   move      vlrpoi        cmzdst
061400010201     C                   eval      cmzfld = vlrfou
061500010201     C                   eval      cmzmbd = vlrhdl
061600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061700021009     C***                if        prmfir = *blanks
061800021017     C                   eval      cmzfla = 'FIVAB00F'
061900021017     C                   eval      cmzmba = 'FIVAB00F'
062000021009     C***                else
062100021009     C***                eval      cmzfla = prmfir
062200021009     C***                eval      cmzmba = prmfir
062300021009     C***                endif
062400010201     C                   eval      cmznrr = *zeros
062500010201     C                   move      §ctrokvb      cmznrr
062600021018     C                   eval      cmzlba = vlrfl1
062700010201     C                   call(e)   'TIS711C'
062800010201     C                   parm                    dscmz
062900010201     C                   parm      *blanks       esito
063000010201     C                   if        %error
063100010201     C                             or cmzerr = '1'
063200010201     C                             or esito  = '1'
063300010201     C                   eval      wrkesito = '3'
063400010201     C                   endif
063500010205     C                   endif
063600990920     C*
063700000613     C                   ENDSR
063800000613     C***
063900070411
064000070411     C     *pssr         BEGSR
064100070411     C*
064200070411     C                   if        %open(tivin00r)
064300070411     C                   close     tivin00r
064400070411     C                   endif
064500070411     C                   if        %open(fivabwwr)
064600070411     C                   close     fivabwwr
064700070411     C                   endif
064800070411     C                   if        %open(fivatwwr)
064900070411     C                   close     fivatwwr
065000070411     C                   endif
065100070411     C*
065200070411     C* Effettuo la chiamata al CLLE preposto
065300070411     C                   call(e)   'TITVVTC'
065400070411     C                   parm                    parccm
065500070411     C                   parm                    parmbr
065600070411     C                   parm      '2'           paropz
065700070411     C*
065800070411     C                   eval      wrkesito = '2'
065900070411     C*
066000070411     C                   seton                                        LR
066100070411     C*
066200070411     C                   ENDSR     '*CANCL'
066300070411     C***
066400070411
066500990910
066600000613     C     *inzsr        BEGSR
066700990910     C*
066800990910     C     *entry        plist
066900990920     C                   parm                    tivlrds
067000990921     C                   parm      wrkesito      esito
067100000724     C                   parm                    prmlit
067200000710     C                   parm                    prmfir
067300000613     C*
067400000830     C* CALCOLA LA DATA CORRENTE
067500000830     C                   time                    wn14             14 0
067600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067700000830     C                   z-add     wn8           g08dat
067800000830     C                   z-add     *zeros        g08inv
067900000830     C                   movel     '0'           g08err
068000000830     C                   call      'XSRDA8'
068100000830     C                   parm                    wlbda8
068200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068300000830     C*
068400000613     C                   ENDSR
068500000613     C***
068600990908
068700031201     Otitv78p   E            riepilogo         2
068800990915     O                                              'Upload via Internet'
068900990915     O                                           +1 'Traduzione TIVIN00R -'
069000021017     O                                           +1 'FIVABWWR/FIVATWWR'
069100010201     O                                           +1 'Report testate bolle'
069200990915     O          E            riepilogo   2
069300990915     O                       wrkdata
069400990915     O                       wrkora              +1
069500990915     O                       procname            +1
069600990915     O          E            riepilogo   2
069700990915     O                                              'Cliente..................:'
069800990915     O                       VABCCM        z     +1
069900990915     O          E            riepilogo   2
070000990920     O                                              'Riferimento Strategi.....:'
070100990920     O                       vlrhdl              +1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Giusti...................:'
070400010201     O                       §CTROKVB      2   +  1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Sbagliati e corretti.....:'
070700971022     O                       §CTRMO        2   +  1
070800990915     O          E            riepilogo   2
070900990915     O                                              'Sbagliati e scartati.....:'
071000971022     O                       §CTRNO        2   +  1
071100000613
071200031201     Otitv78ps  E            testdett          2
071300000613     O                                              'Upload via Internet'
071400000613     O                                           +1 'Traduzione TIVIN00R -'
071500021017     O                                           +1 'FIVABWWR/FIVATWWR'
071600010201     O                                           +1 'Report testate bolle'
071700000616     O          E            testdett    3
071800000613     O                                           +2 'N° rec'
071900000613     O                                           +3 'Anteprima contenuto'
072000000616     O          E            rigadett    2
072100000613     O                       rrnum               +2
072200000621     O                       recko               +3
072300000616     O          E            findett     2
072400000613     O                       wrkdata
072500000613     O                       wrkora              +1
072600000613     O                       procname            +1
072700000616     O          E            findett     2
072800000613     O                                              'Cliente..................:'
072900000613     O                       VABCCM        z     +1
073000000616     O          E            findett     2
073100000613     O                                              'Riferimento Strategi.....:'
073200000613     O                       vlrhdl              +1
073300000616     O          E            findett     2
073400000613     O                                              'Giusti...................:'
073500010201     O                       §CTROKVB      2   +  1
073600000616     O          E            findett     2
073700000613     O                                              'Sbagliati e corretti.....:'
073800000613     O                       §CTRMO        2   +  1
073900000616     O          E            findett     2
074000000613     O                                              'Sbagliati e scartati.....:'
074100000613     O                       §CTRNO        2   +  1
074200000512** CMD - COMANDI CL
074300031201OVRPRTF FILE(TITV78P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
074400031201OVRPRTF FILE(TITV78PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074500031201DLTOVR FILE(TITV78P TITV78PS) LVL(*)
074600000512
074700000512
