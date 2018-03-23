000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800050317     Ftitvl1p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050317     Ftitvl1ps  O    f  198        PRINTER usropn
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
002600050321     D trul33ds      e ds                  inz
002700050321     D kpjba         e ds                  inz
002800990910     D esito           s              1
002900000724     D prmlit          s             10
003000000710     D prmfir          s             10
003100990921     D wrkesito        s                   like(esito)
003200990915     D wrkdata         s               d
003300990915     D wrkora          s               t
003400000613     D rrnum           s              6  0 INZ(*zeros)
003500000621     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700030415     D depspe          s              7    INZ(*blanks)
003800030415     D curspe          s              7    INZ(*blanks)
003900030415     D cntspe          s              7  0 INZ(*zeros)
004000010202     D parccm          s              8    INZ(*blanks)
004100010202     D parmbr          s             10    INZ(*blanks)
004200010202     D paropz          s              1    INZ(*blanks)
004300010202     D chkcall         s              1    INZ(*blanks)
004400010213
004500000801     D  Num5_0         s              5  0
004600030107     D  Num7_0         s              7  0
004700000830
004800000830     D*------------------
004900000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000000830     D*------------------
005100000830     D WLBDA8          DS                  INZ
005200000830     D  G08DAT                 1      8  0
005300000830     D  G08INV                 9     16  0
005400000830     D  G08ERR                17     17
005500000830     D  G08TGI                18     22  0
005600051219     D*------------------
005700051219     D* Costanti
005800051219     D*------------------
005900051219     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006000051219     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006100051219     D*
006200051219
006300010201
006400990915     C                   time                    wrkdata
006500990915     C                   time                    wrkora
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900000724     C*
007000000724     C* SE OCCORRE SPEDIRE IN FILIALE
007100021017     C                   if        vlrpoi <> *zeros
007200000724     C*
007300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007500021017     C     vlrpoi        chain     azorg01l
007600000724     C                   if        %found
007700000616     C                   movel(p)  CMD(1)        depcmd
007800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000724     C*
008600000724     C                   endif
008700000724     C                   endif
008800000616     C*
008900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009000000616     C                   movel(p)  CMD(2)        depcmd
009100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009200000616     C*
009300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009400011113     C                   Z-ADD     150           LENGH            15 5
009500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000616     C                   PARM                    depcmd
009700000616     C                   PARM                    LENGH
009800000616     C*
009900050317     C                   if        not %open(titvl1ps)
010000050317     C                   open      titvl1ps
010100000616     C                   except    testdett
010200000613     C                   endif
010300000613     C*
010400030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010600000613     C*
010700010202     C* Effettuo la chiamata al CLLE preposto
010800031201     C                   call(e)   'TITVVTC'
010900010202     C                   parm                    parccm
011000010202     C                   parm                    parmbr
011100010202     C                   parm      '2'           paropz
011200010202     C*
011300050317     C                   if        %open(titvl1ps)
011400000616     C                   except    findett
011500050317     C                   close     titvl1ps
011600000613     C                   endif
011700000616     C*
011800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    CMD(3)
012200000616     C                   PARM                    LENGH
012300000616     C*
012400010201     C                   seton                                        LR
012500000613
012600000613
012700000613     C*--------------------------------------------------------
012800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012900000613     C*--------------------------------------------------------
013000000621     C     STPR          BEGSR
013100000613     C*
013200050317     C                   if        not %open(titvl1p)
013300050317     C                   open      titvl1p
013400990915     C                   endif
013500990915     C*
013600990915     C                   except    riepilogo
013700990915     C*
013800050317     C                   if        %open(titvl1p)
013900050317     C                   close     titvl1p
014000990914     C                   endif
014100990910     C*
014200000613     C                   ENDSR
014300000613     C***
014400990908
014500000801
014600910830     C*--------------------------------------------------------
014700030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014800910830     C*--------------------------------------------------------
014900030226     C     RWFILE        BEGSR
015000990910     C*
015100990914     C                   if        not %open(tivin00r)
015200990908     C                   open      tivin00r
015300990914     C                   endif
015400021017     C                   if        not %open(fivabwwr)
015500021017     C                   open      fivabwwr
015600990914     C                   endif
015700021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015800020305     C                   exsr      prevat
015900010201     C*
016000010202     C                   if        chkcall = '0'
016100010202     C*
016200021017     C                   if        not %open(fivatwwr)
016300021017     C                   open      fivatwwr
016400010201     C                   endif
016500990910     C*
016600010201     C                   clear                   §CTROKVB          5 0
016700020305     C                   clear                   §CTROKVT          5 0
016800000801     C                   clear                   §CTRMO            5 0
016900000801     C                   clear                   §CTRNO            5 0
017000990910     C*
017100921023     C                   DO        *HIVAL
017200990913     C*
017300990915     C                   READ      tivin00r                               70
017400000905     C                   if        vindta > *blanks
017500000613     C                   add       1             rrnum
017600000801     C*
017700000801     C                   if        *in70 = *off
017800000801     C                             and
017900000801     C                             (vinflg = *blanks
018000000801     C                              or vinflg = '0'
018100000801     C                              or vinflg = '2')
018200000801     C*
018300000801     C                   clear                   vinmsg
018400030310     C                   eval      vinflg = '1'
018500030310     C*
018600030310     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018700050317     C                   eval      PiStr=%trim(%subst(vindta:1:6))
018800030411     C                   movel(p)  PiStr         curspe
018900030411     C                   add       1             cntspe
019000030411     C*
019100030411     C                   if        cntspe = 1
019200030411     C* A rottura inizializzo campi di totalizzazione
019300030411     C                   clear                   fivab000
019400030411     C*
019500030411     C                   exsr      impvab                                       => carico VAB
019600031201     C                   exsr      exevat                                       => carico VAT
019700030411     C                   eval      depspe = curspe
019800030411     C                   else
019900030411     C                   if        curspe <> depspe
020000030411     C                   exsr      wrivab                                       => scarico VAB
020100030411     C* A rottura inizializzo campi di totalizzazione
020200030411     C                   clear                   fivab000
020300030411     C*
020400030411     C                   exsr      impvab                                       => carico VAB
020500031201     C                   exsr      exevat                                       => carico VAT
020600030411     C                   eval      depspe = curspe
020700030411     C                   else
020800030411     C                   exsr      impvab                                       => carico VAB
020900031201     C                   exsr      exevat                                       => carico VAT
021000030411     C                   endif
021100030411     C                   endif
021200000905     C*
021300000905     C                   else
021400000905     C                   eval      vinflg = '1'
021500000905     C                   endif
021600030411     C                   endif
021700000905     C*
021800000905     C  N70              update    tivin000
021900000905     C*
022000991022     C  N70              ENDdo
022100030430     C*
022200030430     C* Al termine della lettura scarico i VAB/VAT ancora in "canna"
022300030430     C                   exsr      wrivab                                       => scarico VAB
022400010202     C*
022500010202     C                   endif
022600990910
022700990910     C* Se non ci sono record con errori ...
022800000710     C                   if        §ctrno = 0
022900990910     C* ... restituisco esito OK.
023000990921     C                   eval      wrkesito = '0'
023100990910     C                   else
023200010201     C                   if        §ctrokvb > 0
023300990921     C                   eval      wrkesito = '1'
023400000710     C                   else
023500000710     C                   eval      wrkesito = '2'
023600990910     C                   endif
023700000710     C                   endif
023800990910     C*
023900990914     C                   if        %open(tivin00r)
024000990908     C                   close     tivin00r
024100990914     C                   endif
024200021017     C                   if        %open(fivabwwr)
024300021017     C                   close     fivabwwr
024400990914     C                   endif
024500021017     C                   if        %open(fivatwwr)
024600021017     C                   close     fivatwwr
024700010201     C                   endif
024800021017     C*
024900021017     C                   if        vlrpoi <> 999
025000021017     C                   eval      vlrpoi = vlrpoi
025100021017     C                   endif
025200990910     C*
025300010201     C                   if        §ctrokvb > 0
025400021017     C                             and vlrpoi > *zeros
025500010202     C                   exsr      invio
025600990920     C                   endif
025700990920     C*
025800910830     C                   ENDSR
025900000613     C***
026000990920
026100000801     C*----------------------------------------------------*
026200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026300000801     C*----------------------------------------------------*
026400010201     C     INZVAR        BEGSR
026500000801     C*
026600010201     C                   Z-ADD     *zeros        Num5_0
026700030107     C                   Z-ADD     *zeros        Num7_0
026800030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
026900030310     C                   MOVEL     '0'           FlgCAS            1
027000000801     C*
027100000801     C                   ENDSR
027200000801     C*----------------------------------------------------*
027300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027400000801     C*----------------------------------------------------*
027500000801     C     DEFCAM        BEGSR
027600000801     C*
027700020619     C* Imposto i valori di default...
027800050317     C                   Z-ADD     0010083       VABCCM
027900050317     C                   Z-ADD     0010083       VATCCM
028000021017     C                   Z-ADD     vlrpoi        VABFGS
028100021017     C                   Z-ADD     vlrpoi        VATFGS
028200050317     C                   Z-ADD     001           VABLNP
028300050317     C                   Z-ADD     001           VATLNP
028400030226     C                   Z-ADD     000           VABCTR
028500021017     C                   MOVEL     '7Q'          VABCTM
028600050317     C                   MOVEL     'C'           VABTSP
028700050321     C                   MOVEL     '1'           VABCBO
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
030700020619     C                   ENDIF
030800000801     C*
030900000801     C                   ENDSR
031000000801     C*----------------------------------------------------*
031100030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
031200000801     C*----------------------------------------------------*
031300010201     C     IMPVAB        BEGSR
031400000801     C*
031500020305     C                   EXSR      INZVAR
031600020305     C                   EXSR      DEFCAM
031700010305     C*
031800030226     C                   Z-ADD     *zeros        errore            1 0
031900000830     C                   MOVEL     datcor        VABAAS
032000000830     C                   MOVE      datcor        VABMGS
032100030226     C*
032200050317     C                   EVAL      VABRSD=%trim(%subst(vindta:33:40))
032300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032500020117     C* ==
032600050317     C                   EVAL      VABIND=%trim(%subst(vindta:73:50))
032700050317     C                   EVAL      VABLOD=%trim(%subst(vindta:123:50))
032800050317     C                   EVAL      VABPRD=%trim(%subst(vindta:173:2))
032900050317     C                   EVAL      VABRMA=%trim(%subst(vindta:326:10))
033000050317
033100050317     C                   EVAL      VABTIC=%trim(%subst(vindta:249:2))
033200050317     C                   EVAL      VABTC1=%trim(%subst(vindta:296:1))
033300050318
033400050318     C                   IF        %trim(%subst(vindta:297:1)) = 'P'
033500050318     C                   EVAL      %subst(VABGC1:2:1) = 'M'
033600050318     C                   ENDIF
033700050318     C                   IF        %trim(%subst(vindta:297:1)) = 'M'
033800050318     C                   EVAL      %subst(VABGC1:2:1) = 'P'
033900050318     C                   ENDIF
034000050613
034100051219     C                   MOVEL     *blanks       wDati            20
034200051219     C                   EVAL      wDati=%trim(%subst(vindta:336:20))
034300051219     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
034400051219     C     minu:maiu     XLATE     wDati         wDati
034500051219     C                   IF        %trim(wDati) = 'ORDINARIO'
034600050613     C                   EVAL      VABTSP = 'C'
034700050613     C                   ENDIF
034800051219     C                   IF        %trim(wDati) = 'ESPRESSO'
034900050613     C                   EVAL      VABTSP = 'E'
035000050613     C                   ENDIF
035100050317
035200051219     C                   EVAL      wDati=%trim(%subst(vindta:256:20))
035300051219     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
035400051219     C     minu:maiu     XLATE     wDati         wDati
035500051219     C                   IF        %scan('FRANCO':wDati) > 0
035600050317     C                   EVAL      VABCBO = '1'
035700050317     C                   ENDIF
035800051219     C                   IF        %scan('ASSEGNATO':wDati) > 0
035900050317     C                   EVAL      VABCBO = '2'
036000050317     C                   ENDIF
036100050317
036200030226     C* CAP
036300050317     C                   EVAL      PiStr=%trim(%subst(vindta:175:5))
036400010201     C                   EXSR      CHKNUM
036500010201     C                   IF        PiInt=*on
036600010201     C                   Z-ADD     PiVal         Num5_0
036700021017     C                   MOVEL     Num5_0        VABCAD
036800010201     C                   ELSE
036900010201     C                   ADD       1             errore
037000010201     C                   EVAL      vinmsg = %trimr(vinmsg)
037100021017     C                             + ' ' + 'VABCAD'
037200010201     C                   ENDIF
037300031201     C* NSP/RMN
037400050317     C                   EVAL      PiStr=%trim(%subst(vindta:1:6))
037500010201     C                   EXSR      CHKNUM
037600050321     C                   IF        PiInt=*on AND PiVal <> *zeros
037700050317     C                   Z-ADD     PiVal         VABNSP
037800050317     C                   Z-ADD     PiVal         VATNSP
037900031201     C                   Z-ADD     PiVal         VABRMN
038000010201     C                   ELSE
038100050321     C                   Z-ADD     1             VABRMN
038200050321     C* NSP => Stacco un numeratore da AZNUM
038300050321     C                   clear                   TRUL33DS
038400050321     C                   eval      I33OPE = *zeros
038500050321     C                   eval      I33CNU = 302
038600050321     C                   eval      I33NUM = 1
038700050321     C                   movel     TRUL33DS      KPJBU
038800050321     C                   call      'TRUL33R'
038900050321     C                   parm                    KPJBA
039000050321     C                   movel     KPJBU         TRUL33DS
039100050321     C                   if        O33ERR = *zeros
039200050321     C                   z-add     O33NRF        VABNSP
039300050321     C                   z-add     O33NRF        VATNSP
039400050321     C                   else
039500050321     C                   z-add     1             errore
039600050321     C                   eval      vinmsg = %trimr(vinmsg)
039700031201     C                             + ' ' + 'VABNSP VATNSP VABRMN'
039800050321     C                   endif
039900010201     C                   ENDIF
040000030226     C* NCL
040100050317     C                   EVAL      PiStr=%trim(%subst(vindta:180:6))
040200050317     C                   EXSR      CHKNUM
040300050317     C                   IF        PiInt=*on
040400050317     C                   Z-ADD     PiVal         VABNCL
040500050317     C                   ELSE
040600050317     C                   ADD       1             errore
040700050317     C                   EVAL      vinmsg = %trimr(vinmsg)
040800050317     C                             + ' ' + 'VABNCL'
040900050317     C                   ENDIF
041000030226     C* PKB
041100050317     C                   EVAL      PiStr=%trim(%subst(vindta:186:13))
041200030107     C                   EXSR      CHKNUM
041300030107     C                   IF        PiNum=*on
041400050317     C                   Z-ADD     PiVal         VABPKB
041500030107     C                   ELSE
041600030107     C                   ADD       1             errore
041700030107     C                   EVAL      vinmsg = %trimr(vinmsg)
041800030107     C                             + ' ' + 'VABPKB'
041900030107     C                   ENDIF
042000050317     C* CAS/VCA
042100050414     C                   IF        %trim(%subst(vindta:276:20)) <> '0,00'
042200050321     C                   MOVEL     '1'           FlgCAS
042300050317     C                   EVAL      PiStr=%trim(%subst(vindta:276:20))
042400050317     C                   EXSR      CHKNUM
042500050317     C                   IF        PiNum=*on
042600050321     C                   Z-ADD     PiVal         VABCAS
042700050317     C                   EVAL      VABVCA=%trim(%subst(vindta:251:5))
042800050317     C                   ELSE
042900050317     C                   SETON                                        32
043000050317     C                   Z-ADD     *zeros        VABCAS
043100050317     C                   EVAL      vinmsg = %trimr(vinmsg)
043200050317     C                             + ' ' + 'VABCAS'
043300050317     C                   ENDIF
043400050321     C                   ENDIF
043500090514     C* QFT
043600090514     C                   IF        %trim(%subst(vindta:356:20)) <> '0,00' AND
043700090514     C                             %trim(%subst(vindta:356:20)) <> *blanks
043800090514     C                   EVAL      VABCTR = 400
043900090512     C                   EVAL      PiStr=%trim(%subst(vindta:356:20))
044000090512     C                   EXSR      CHKNUM
044100090512     C                   IF        PiNum=*on
044200090514     C                   Z-ADD     PiVal         VABQFT
044300090512     C                   ELSE
044400090512     C                   SETON                                        32
044500090514     C                   Z-ADD     *zeros        VABQFT
044600090512     C                   EVAL      vinmsg = %trimr(vinmsg)
044700090514     C                             + ' ' + 'VABQFT'
044800090512     C                   ENDIF
044900090512     C                   ENDIF
045000030310     C*
045100050321     C* Considerazioni finali su CBO/CAS
045200030310     C                   IF        FlgCAS = '1'
045300050321     C                   IF        VABCBO = '1'
045400010205     C                   EVAL      VABCBO = '4'
045500050321     C                   ENDIF
045600050321     C                   IF        VABCBO = '2'
045700050321     C                   EVAL      VABCBO = '6'
045800050321     C                   ENDIF
045900010205     C                   ENDIF
046000020305     C*
046100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
046200011113     C                   EXSR      CHKIMPDIV
046300010202     C*
046400000801     C* Ebbene...
046500000801     C                   ADD       1             §CTRMO
046600010201     C                   IF        errore <> *zeros
046700000801     C                   ADD       1             §CTRNO
046800000801     C                   EVAL      recko = vindta
046900000801     C                   EXCEPT    rigadett
047000000801     C                   EVAL      vinflg = '2'
047100000801     C                   ELSE
047200010201     C                   ADD       1             §CTROKVB
047300000801     C                   ENDIF
047400000801     C*
047500000801     C                   ENDSR
047600030226     C*----------------------------------------------------*
047700030226     C*  SCARICAMENTO BUFFER RECORDS VAB
047800030226     C*----------------------------------------------------*
047900030226     C     WRIVAB        BEGSR
048000030310     C*
048100030310     C                   write     FIVAB000
048200030226     C*
048300030226     C                   ENDSR
048400030226     C*----------------------------------------------------*
048500030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
048600030226     C*----------------------------------------------------*
048700031201     C     EXEVAT        BEGSR
048800030226     C*
048900030226     C                   MOVEL     datcor        VATAAS
049000030226     C*
049100030415     C* Valorizzo il barcode
049200031201     C                   EVAL      VATTRC='E'
049300050317     C                   EVAL      VATNOT=%trim(%subst(vindta:298:28))
049400031201     C                   exsr      wrivat                                       => scarico VAT
049500030226     C*
049600030226     C                   ENDSR
049700010201     C*----------------------------------------------------*
049800021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
049900010201     C*----------------------------------------------------*
050000020305     C     WRIVAT        BEGSR
050100010201     C*
050200021017     C                   write     FIVAT000
050300010201     C*
050400010201     C                   ENDSR
050500010202     C*----------------------------------------------------*
050600021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050700010202     C*----------------------------------------------------*
050800020305     C     PREVAT        BEGSR
050900010202     C*
051000021017     C* Compongo il nome del membro da dare al FIVATWWR
051100010202     C                   eval      parmbr = vlrhdl
051200010202     C                   movel     'M'           parmbr
051300050317     C                   eval      parccm = %subst(vlrKSC:2:7)
051400010202     C                   eval      paropz = '1'
051500010202     C* Effettuo la chiamata al CLLE preposto
051600031201     C                   call(e)   'TITVVTC'
051700010202     C                   parm                    parccm
051800010202     C                   parm                    parmbr
051900010202     C                   parm                    paropz
052000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052100010202     C                   if        %error
052200010202     C                   movel     '1'           chkcall
052300010202     C                   else
052400010202     C                   movel     '0'           chkcall
052500010202     C                   endif
052600010202     C*
052700010202     C                   ENDSR
052800000801     C*----------------------------------------------------*
052900000801     C*  CONTROLLO NUMERICITA' CAMPI
053000000801     C*----------------------------------------------------*
053100000801     C     CHKNUM        BEGSR
053200000801     C*
053300000801     C                   call(e)   'ISNUMERIC'
053400000801     C                   PARM                    PiStr            30
053500030107     C                   PARM      ','           PiDecChr          1
053600000801     C                   PARM      *ZEROS        PiVal            30 9
053700000801     C                   PARM      '0'           PiInt             1
053800000801     C                   PARM      '0'           PiNum             1
053900000801     C                   IF        %error
054000000801     C                   EVAL      PiInt=*off
054100030107     C                   EVAL      PiNum=*off
054200000801     C                   ENDIF
054300000801     C*
054400000801     C                   ENDSR
054500000801     C***
054600000801
054700011113
054800011113     C*----------------------------------------------------*
054900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
055000011113     C*----------------------------------------------------*
055100011113     C     CHKIMPDIV     BEGSR
055200011113     C*
055300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
055400011113     C                   Z-ADD     *zeros        wrkDec            9 9
055500011113     C*
055600011113     C* Come prima cosa effettuo considerazioni sulla divisa
055700011113     C                   IF        vabIAS > *zeros
055800011113     C                   IF        vabVAS <> 'EUR'
055900011113     C                   EVAL      vabVAS =  'ITL'
056000011113     C                   ENDIF
056100011113     C                   ENDIF
056200011113     C*
056300011113     C                   IF        vabCAS > *zeros
056400011113     C                   IF        vabVCA <> 'EUR'
056500011113     C                   EVAL      vabVCA =  'ITL'
056600011113     C                   ENDIF
056700011113     C                   ENDIF
056800011113     C*
056900011113     C                   IF        vabVMD > *zeros
057000020305     C                   IF        vabVAD <> 'EUR'
057100011113     C                   EVAL      vabVAD =  'ITL'
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
057600011113     C                   Z-ADD     vabIAS        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVAS = 'ITL'
057900011113     C                   EVAL      vabIAS = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
058400011113     C                   Z-ADD     vabCAS        wrkDec
058500011113     C                   IF        wrkDec > *zeros
058600011113     C                   IF        vabVCA = 'ITL'
058700011113     C                   EVAL      vabCAS = *zeros
058800011113     C                   ENDIF
058900011113     C                   ENDIF
059000011113     C*
059100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
059200011113     C                   Z-ADD     vabVMD        wrkDec
059300011113     C                   IF        wrkDec > *zeros
059400011113     C                   IF        vabVAD = 'ITL'
059500011113     C                   EVAL      vabVMD = *zeros
059600011113     C                   ENDIF
059700011113     C                   ENDIF
059800011113     C*
059900011113     C                   ENDSR
060000011113     C***
060100021017
060200000801
060300000801
060400990920      /TITLE Invio dei dati al punto operativo.
060500010202     C     invio         BEGSR
060600990920     C*
060700021017     C* 1° invio FIVAT
060800010201     C                   reset                   dscmz
060900021017     C                   move      vlrpoi        cmzdst
061000021017     C                   eval      cmzfld = 'FIVATWWR'
061100010201     C                   eval      cmzmbd = vlrhdl
061200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061300021017     C                   eval      cmzfla = 'FIVAT00F'
061400021017     C                   eval      cmzmba = 'FIVAT00F'
061500010201     C                   eval      cmznrr = *zeros
061600020305     C                   move      §ctrokvt      cmznrr
061700021018     C                   eval      cmzlba = vlrfl1
061800010201     C                   call(e)   'TIS711C'
061900010201     C                   parm                    dscmz
062000010201     C                   parm      *blanks       esito
062100010205     C                   if        %error
062200010205     C                             or cmzerr = '1'
062300010205     C                             or esito  = '1'
062400010205     C                   eval      wrkesito = '3'
062500010205     C                   else
062600010201     C*
062700021017     C* 2° invio FIVAB
062800010201     C                   reset                   dscmz
062900021017     C                   move      vlrpoi        cmzdst
063000010201     C                   eval      cmzfld = vlrfou
063100010201     C                   eval      cmzmbd = vlrhdl
063200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
063300021017     C                   eval      cmzfla = 'FIVAB00F'
063400021017     C                   eval      cmzmba = 'FIVAB00F'
063500010201     C                   eval      cmznrr = *zeros
063600010201     C                   move      §ctrokvb      cmznrr
063700021018     C                   eval      cmzlba = vlrfl1
063800010201     C                   call(e)   'TIS711C'
063900010201     C                   parm                    dscmz
064000010201     C                   parm      *blanks       esito
064100010201     C                   if        %error
064200010201     C                             or cmzerr = '1'
064300010201     C                             or esito  = '1'
064400010201     C                   eval      wrkesito = '3'
064500010201     C                   endif
064600010205     C                   endif
064700990920     C*
064800000613     C                   ENDSR
064900000613     C***
065000070411
065100070411     C     *pssr         BEGSR
065200070411     C*
065300070411     C                   if        %open(tivin00r)
065400070411     C                   close     tivin00r
065500070411     C                   endif
065600070411     C                   if        %open(fivabwwr)
065700070411     C                   close     fivabwwr
065800070411     C                   endif
065900070411     C                   if        %open(fivatwwr)
066000070411     C                   close     fivatwwr
066100070411     C                   endif
066200070411     C*
066300070411     C* Effettuo la chiamata al CLLE preposto
066400070411     C                   call(e)   'TITVVTC'
066500070411     C                   parm                    parccm
066600070411     C                   parm                    parmbr
066700070411     C                   parm      '2'           paropz
066800070411     C*
066900070411     C                   eval      wrkesito = '2'
067000070411     C*
067100070411     C                   seton                                        LR
067200070411     C*
067300070411     C                   ENDSR     '*CANCL'
067400070411     C***
067500070411
067600990910
067700000613     C     *inzsr        BEGSR
067800990910     C*
067900990910     C     *entry        plist
068000990920     C                   parm                    tivlrds
068100990921     C                   parm      wrkesito      esito
068200000724     C                   parm                    prmlit
068300000710     C                   parm                    prmfir
068400000613     C*
068500000830     C* CALCOLA LA DATA CORRENTE
068600000830     C                   time                    wn14             14 0
068700000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068800000830     C                   z-add     wn8           g08dat
068900000830     C                   z-add     *zeros        g08inv
069000000830     C                   movel     '0'           g08err
069100000830     C                   call      'XSRDA8'
069200000830     C                   parm                    wlbda8
069300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069400000830     C*
069500000613     C                   ENDSR
069600000613     C***
069700990908
069800050317     Otitvl1p   E            riepilogo         2
069900990915     O                                              'Upload via Internet'
070000990915     O                                           +1 'Traduzione TIVIN00R -'
070100021017     O                                           +1 'FIVABWWR/FIVATWWR'
070200010201     O                                           +1 'Report testate bolle'
070300990915     O          E            riepilogo   2
070400990915     O                       wrkdata
070500990915     O                       wrkora              +1
070600990915     O                       procname            +1
070700990915     O          E            riepilogo   2
070800990915     O                                              'Cliente..................:'
070900990915     O                       VABCCM        z     +1
071000990915     O          E            riepilogo   2
071100990920     O                                              'Riferimento Strategi.....:'
071200990920     O                       vlrhdl              +1
071300990915     O          E            riepilogo   2
071400990915     O                                              'Giusti...................:'
071500010201     O                       §CTROKVB      2   +  1
071600990915     O          E            riepilogo   2
071700990915     O                                              'Sbagliati e corretti.....:'
071800971022     O                       §CTRMO        2   +  1
071900990915     O          E            riepilogo   2
072000990915     O                                              'Sbagliati e scartati.....:'
072100971022     O                       §CTRNO        2   +  1
072200000613
072300050317     Otitvl1ps  E            testdett          2
072400000613     O                                              'Upload via Internet'
072500000613     O                                           +1 'Traduzione TIVIN00R -'
072600021017     O                                           +1 'FIVABWWR/FIVATWWR'
072700010201     O                                           +1 'Report testate bolle'
072800000616     O          E            testdett    3
072900000613     O                                           +2 'N° rec'
073000000613     O                                           +3 'Anteprima contenuto'
073100000616     O          E            rigadett    2
073200000613     O                       rrnum               +2
073300000621     O                       recko               +3
073400000616     O          E            findett     2
073500000613     O                       wrkdata
073600000613     O                       wrkora              +1
073700000613     O                       procname            +1
073800000616     O          E            findett     2
073900000613     O                                              'Cliente..................:'
074000000613     O                       VABCCM        z     +1
074100000616     O          E            findett     2
074200000613     O                                              'Riferimento Strategi.....:'
074300000613     O                       vlrhdl              +1
074400000616     O          E            findett     2
074500000613     O                                              'Giusti...................:'
074600010201     O                       §CTROKVB      2   +  1
074700000616     O          E            findett     2
074800000613     O                                              'Sbagliati e corretti.....:'
074900000613     O                       §CTRMO        2   +  1
075000000616     O          E            findett     2
075100000613     O                                              'Sbagliati e scartati.....:'
075200000613     O                       §CTRNO        2   +  1
075300000512** CMD - COMANDI CL
075400050317OVRPRTF FILE(TITVL1P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
075500050317OVRPRTF FILE(TITVL1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
075600050317DLTOVR FILE(TITVL1P TITVL1PS) LVL(*)
075700000512
075800000512
