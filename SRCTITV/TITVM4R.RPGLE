000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800050419     Ftitvm4p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050419     Ftitvm4ps  O    f  198        PRINTER usropn
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
003700010202     D parccm          s              8    INZ(*blanks)
003800010202     D parmbr          s             10    INZ(*blanks)
003900010202     D paropz          s              1    INZ(*blanks)
004000010202     D chkcall         s              1    INZ(*blanks)
004100010213
004200000801     D  Num5_0         s              5  0
004300030107     D  Num7_0         s              7  0
004400050419
004500050419     d wnumbolla       s              1
004600050419     d wnsp            s              6
004700050419     d wbar            s             13
004800050419     D wBARCODE        s             18    inz(*blanks)
004900050419     d xx              s              4  0
005000050419     d wncl            s              4
005100090916     d Trul28dsx     e ds
005200000830
005300000830     D*------------------
005400000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500000830     D*------------------
005600000830     D WLBDA8          DS                  INZ
005700000830     D  G08DAT                 1      8  0
005800000830     D  G08INV                 9     16  0
005900000830     D  G08ERR                17     17
006000000830     D  G08TGI                18     22  0
006100000830     D*
006200990908
006300010201
006400010201
006500990915     C                   time                    wrkdata
006600990915     C                   time                    wrkora
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000724     C*
007100000724     C* SE OCCORRE SPEDIRE IN FILIALE
007200021017     C                   if        vlrpoi <> *zeros
007300000724     C*
007400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007600021017     C     vlrpoi        chain     azorg01l
007700000724     C                   if        %found
007800000616     C                   movel(p)  CMD(1)        depcmd
007900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000724     C*
008700000724     C                   endif
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000616     C*
010000050419     C                   if        not %open(titvm4ps)
010100050419     C                   open      titvm4ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400000613     C*
010500030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010700000613     C*
010800010202     C* Effettuo la chiamata al CLLE preposto
010900031201     C                   call(e)   'TITVVTC'
011000010202     C                   parm                    parccm
011100010202     C                   parm                    parmbr
011200010202     C                   parm      '2'           paropz
011300010202     C*
011400050419     C                   if        %open(titvm4ps)
011500000616     C                   except    findett
011600050419     C                   close     titvm4ps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400000616     C*
012500010201     C                   seton                                        LR
012600000613
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300050419     C                   if        not %open(titvm4p)
013400050419     C                   open      titvm4p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900050419     C                   if        %open(titvm4p)
014000050419     C                   close     titvm4p
014100990914     C                   endif
014200990910     C*
014300000613     C                   ENDSR
014400000613     C***
014500990908
014600000801
014700910830     C*--------------------------------------------------------
014800030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014900910830     C*--------------------------------------------------------
015000030226     C     RWFILE        BEGSR
015100990910     C*
015200990914     C                   if        not %open(tivin00r)
015300990908     C                   open      tivin00r
015400990914     C                   endif
015500021017     C                   if        not %open(fivabwwr)
015600021017     C                   open      fivabwwr
015700990914     C                   endif
015800021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015900020305     C                   exsr      prevat
016000010201     C*
016100010202     C                   if        chkcall = '0'
016200010202     C*
016300021017     C                   if        not %open(fivatwwr)
016400021017     C                   open      fivatwwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   clear                   §CTROKVB          5 0
016800020305     C                   clear                   §CTROKVT          5 0
016900000801     C                   clear                   §CTRMO            5 0
017000000801     C                   clear                   §CTRNO            5 0
017100990910     C*
017200921023     C                   DO        *HIVAL
017300990913     C*
017400990915     C                   READ      tivin00r                               70
017500000905     C                   if        vindta > *blanks
017600000613     C                   add       1             rrnum
017700000801     C*
017800000801     C                   if        *in70 = *off
017900000801     C                             and
018000000801     C                             (vinflg = *blanks
018100000801     C                              or vinflg = '0'
018200000801     C                              or vinflg = '2')
018300000801     C*
018400000801     C                   clear                   vinmsg
018500030310     C                   eval      vinflg = '1'
018600030411     C*
018700030411     C                   clear                   fivab000
018800030411     C*
018900030411     C                   exsr      impvab                                       => carico VAB
019000050419      * i record del VAT li genero solo se il numero di spedizione è stato passato
019100050419      * nel file
019200050419     c                   If        wnumbolla = *Blanks
019300031201     C                   exsr      exevat                                       => carico VAT
019400050419     c                   EndIf
019500030411     C                   exsr      wrivab                                       => scarico VAB
019600000905     C*
019700000905     C                   else
019800000905     C                   eval      vinflg = '1'
019900000905     C                   endif
020000030411     C                   endif
020100000905     C*
020200000905     C  N70              update    tivin000
020300000905     C*
020400991022     C  N70              ENDdo
020500010202     C*
020600010202     C                   endif
020700990910
020800990910     C* Se non ci sono record con errori ...
020900000710     C                   if        §ctrno = 0
021000990910     C* ... restituisco esito OK.
021100990921     C                   eval      wrkesito = '0'
021200990910     C                   else
021300010201     C                   if        §ctrokvb > 0
021400990921     C                   eval      wrkesito = '1'
021500000710     C                   else
021600000710     C                   eval      wrkesito = '2'
021700990910     C                   endif
021800000710     C                   endif
021900990910     C*
022000990914     C                   if        %open(tivin00r)
022100990908     C                   close     tivin00r
022200990914     C                   endif
022300021017     C                   if        %open(fivabwwr)
022400021017     C                   close     fivabwwr
022500990914     C                   endif
022600021017     C                   if        %open(fivatwwr)
022700021017     C                   close     fivatwwr
022800010201     C                   endif
022900021017     C*
023000021017     C                   if        vlrpoi <> 999
023100021017     C                   eval      vlrpoi = vlrpoi
023200021017     C                   endif
023300990910     C*
023400010201     C                   if        §ctrokvb > 0
023500021017     C                             and vlrpoi > *zeros
023600010202     C                   exsr      invio
023700990920     C                   endif
023800990920     C*
023900910830     C                   ENDSR
024000000613     C***
024100990920
024200000801     C*----------------------------------------------------*
024300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024400000801     C*----------------------------------------------------*
024500010201     C     INZVAR        BEGSR
024600000801     C*
024700010201     C                   Z-ADD     *zeros        Num5_0
024800030107     C                   Z-ADD     *zeros        Num7_0
024900050419      * pulisco flag che mi dice se c'è il numero di spedizione nel file o meno
025000050419     c                   Clear                   wnumbolla
025100000801     C*
025200000801     C                   ENDSR
025300000801     C*----------------------------------------------------*
025400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025500000801     C*----------------------------------------------------*
025600000801     C     DEFCAM        BEGSR
025700000801     C*
025800020619     C* Imposto i valori di default...
025900050419     C                   Z-ADD     0240425       VABCCM
026000050419     C                   Z-ADD     0240425       VATCCM
026100021017     C                   Z-ADD     vlrpoi        VABFGS
026200021017     C                   Z-ADD     vlrpoi        VATFGS
026300050419     C                   Z-ADD     024           VABLNP
026400050419     C                   Z-ADD     024           VATLNP
026500030226     C                   Z-ADD     000           VABCTR
026600050317     C                   MOVEL     'C'           VABTSP
026700050321     C                   MOVEL     '1'           VABCBO
026800020619     C* ... e poi verifico se sono stati passati come parametri
026900020619     C                   IF        vlrppt > *blanks
027000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
027100020619     C                   EXSR      CHKNUM
027200020619     C                   IF        PiInt=*on
027300020619     C                   Z-ADD     PiVal         VABCCM
027400020619     C                   Z-ADD     PiVal         VATCCM
027500020619     C                   ENDIF
027600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027700020619     C                   EXSR      CHKNUM
027800020619     C                   IF        PiInt=*on
027900020619     C                   Z-ADD     PiVal         VABLNP
028000020619     C                   Z-ADD     PiVal         VATLNP
028100020619     C                   ENDIF
028200020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028300020619     C                   EXSR      CHKNUM
028400020619     C                   IF        PiInt=*on
028500020619     C                   Z-ADD     PiVal         VABCTR
028600020619     C                   ENDIF
028700020619     C                   ENDIF
028800000801     C*
028900000801     C                   ENDSR
029000000801     C*----------------------------------------------------*
029100030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
029200000801     C*----------------------------------------------------*
029300010201     C     IMPVAB        BEGSR
029400000801     C*
029500020305     C                   EXSR      INZVAR
029600020305     C                   EXSR      DEFCAM
029700010305     C*
029800030226     C                   Z-ADD     *zeros        errore            1 0
029900000830     C                   MOVEL     datcor        VABAAS
030000000830     C                   MOVE      datcor        VABMGS
030100030226     C*
030200050419     C                   EVAL      VABRSD=%trim(%subst(vindta:7:35))
030300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030500020117     C* ==
030600050419     C                   EVAL      VABIND=%trim(%subst(vindta:42:35))
030700050419     C                   EVAL      VABLOD=%trim(%subst(vindta:82:30))
030800050419     C                   EVAL      VABPRD=%trim(%subst(vindta:112:2))
030900050419     C                   EVAL      VABRMA=%trim(%subst(vindta:1:6))
031000050317
031100030226     C* CAP
031200050419     C                   EVAL      PiStr=%trim(%subst(vindta:77:5))
031300010201     C                   EXSR      CHKNUM
031400010201     C                   IF        PiInt=*on
031500010201     C                   Z-ADD     PiVal         Num5_0
031600021017     C                   MOVEL     Num5_0        VABCAD
031700010201     C                   ELSE
031800010201     C                   ADD       1             errore
031900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032000021017     C                             + ' ' + 'VABCAD'
032100010201     C                   ENDIF
032200050419     C* NSP
032300050419     C                   EVAL      PiStr=%trim(%subst(vindta:129:7))
032400010201     C                   EXSR      CHKNUM
032500050321     C                   IF        PiInt=*on AND PiVal <> *zeros
032600050317     C                   Z-ADD     PiVal         VABNSP
032700050317     C                   Z-ADD     PiVal         VATNSP
032800010201     C                   ELSE
032900050419     c                   Eval      wnumbolla = 'N'
033000050321     C* NSP => Stacco un numeratore da AZNUM
033100050321     C                   clear                   TRUL33DS
033200050321     C                   eval      I33OPE = *zeros
033300050321     C                   eval      I33CNU = 302
033400050321     C                   eval      I33NUM = 1
033500050321     C                   movel     TRUL33DS      KPJBU
033600050321     C                   call      'TRUL33R'
033700050321     C                   parm                    KPJBA
033800050321     C                   movel     KPJBU         TRUL33DS
033900050321     C                   if        O33ERR = *zeros
034000050321     C                   z-add     O33NRF        VABNSP
034100050321     C                   else
034200050321     C                   z-add     1             errore
034300050321     C                   eval      vinmsg = %trimr(vinmsg)
034400050419     C                             + ' ' + 'VABNSP VATNSP'
034500050321     C                   endif
034600010201     C                   ENDIF
034700030226     C* NCL
034800050419     C                   EVAL      PiStr=%trim(%subst(vindta:114:4))
034900050317     C                   EXSR      CHKNUM
035000050317     C                   IF        PiInt=*on
035100050317     C                   Z-ADD     PiVal         VABNCL
035200050317     C                   ELSE
035300050317     C                   ADD       1             errore
035400050317     C                   EVAL      vinmsg = %trimr(vinmsg)
035500050317     C                             + ' ' + 'VABNCL'
035600050317     C                   ENDIF
035700030226     C* PKB
035800050419     C                   EVAL      PiStr=%trim(%subst(vindta:118:4))
035900030107     C                   EXSR      CHKNUM
036000030107     C                   IF        PiNum=*on
036100050317     C                   Z-ADD     PiVal         VABPKB
036200030107     C                   ELSE
036300030107     C                   ADD       1             errore
036400030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036500030107     C                             + ' ' + 'VABPKB'
036600030107     C                   ENDIF
036700050419     C* RMN
036800050419     C                   EVAL      PiStr=%trim(%subst(vindta:122:7))
036900050419     C                   EXSR      CHKNUM
037000050419     C                   IF        PiInt=*on
037100050419     C                   Z-ADD     PiVal         VABRMN
037200050419     C                   ELSE
037300050419     C                   ADD       1             errore
037400050419     C                   EVAL      vinmsg = %trimr(vinmsg)
037500050419     C                             + ' ' + 'VABRMN'
037600050419     C                   ENDIF
037700050419      * se il numero di spedizione è stato passato nel file il codice trattamento merce è 7Q
037800050419      * altrimenti se il numero spedizione l'ho recuperato io il codice trattamento merce è 7K
037900050419     c                   If        wnumbolla = *Blanks
038000050419     c                   Eval      VABCTM = '7Q'
038100050419     c                   Else
038200050419     c                   Eval      VABCTM = '7K'
038300050419     c                   EndIf
038400020305     C*
038500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038600011113     C                   EXSR      CHKIMPDIV
038700010202     C*
038800000801     C* Ebbene...
038900000801     C                   ADD       1             §CTRMO
039000010201     C                   IF        errore <> *zeros
039100000801     C                   ADD       1             §CTRNO
039200000801     C                   EVAL      recko = vindta
039300000801     C                   EXCEPT    rigadett
039400000801     C                   EVAL      vinflg = '2'
039500000801     C                   ELSE
039600010201     C                   ADD       1             §CTROKVB
039700000801     C                   ENDIF
039800000801     C*
039900000801     C                   ENDSR
040000030226     C*----------------------------------------------------*
040100030226     C*  SCARICAMENTO BUFFER RECORDS VAB
040200030226     C*----------------------------------------------------*
040300030226     C     WRIVAB        BEGSR
040400030310     C*
040500030310     C                   write     FIVAB000
040600030226     C*
040700030226     C                   ENDSR
040800030226     C*----------------------------------------------------*
040900030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
041000030226     C*----------------------------------------------------*
041100031201     C     EXEVAT        BEGSR
041200030226     C*
041300030226     C                   MOVEL     datcor        VATAAS
041400030226     C*
041500030415     C* Valorizzo il barcode
041600031201     C                   EVAL      VATTRC='E'
041700050419      * x ogni collo devo generare una riga di VAT
041800050419      * compongo il segnacollo con vabnsp(ultimi 6) + 999(fisso) + n.collo + check digit
041900050419     c                   Do        vabncl        xx
042000050419     c                   Eval      wnsp = %trim(%subst(vindta:130:6))
042100050419     c                   Move      xx            wncl
042200050419     c                   Eval      wbar = wnsp + '999' + wncl
042300050419     C* X ogni segnacollo costruisco il barcode standard BARTOLINI
042400050419     C                   exsr      calbarcode
042500050419     C                   EVAL      VATNOT=wbarcode
042600031201     C                   exsr      wrivat                                       => scarico VAT
042700050419     c                   EndDo
042800030226     C*
042900030226     C                   ENDSR
043000010201     C*----------------------------------------------------*
043100021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
043200010201     C*----------------------------------------------------*
043300020305     C     WRIVAT        BEGSR
043400010201     C*
043500021017     C                   write     FIVAT000
043600010201     C*
043700010201     C                   ENDSR
043800010202     C*----------------------------------------------------*
043900021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044000010202     C*----------------------------------------------------*
044100020305     C     PREVAT        BEGSR
044200010202     C*
044300021017     C* Compongo il nome del membro da dare al FIVATWWR
044400010202     C                   eval      parmbr = vlrhdl
044500010202     C                   movel     'M'           parmbr
044600050317     C                   eval      parccm = %subst(vlrKSC:2:7)
044700010202     C                   eval      paropz = '1'
044800010202     C* Effettuo la chiamata al CLLE preposto
044900031201     C                   call(e)   'TITVVTC'
045000010202     C                   parm                    parccm
045100010202     C                   parm                    parmbr
045200010202     C                   parm                    paropz
045300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045400010202     C                   if        %error
045500010202     C                   movel     '1'           chkcall
045600010202     C                   else
045700010202     C                   movel     '0'           chkcall
045800010202     C                   endif
045900010202     C*
046000010202     C                   ENDSR
046100000801     C*----------------------------------------------------*
046200000801     C*  CONTROLLO NUMERICITA' CAMPI
046300000801     C*----------------------------------------------------*
046400000801     C     CHKNUM        BEGSR
046500000801     C*
046600000801     C                   call(e)   'ISNUMERIC'
046700000801     C                   PARM                    PiStr            30
046800030107     C                   PARM      ','           PiDecChr          1
046900000801     C                   PARM      *ZEROS        PiVal            30 9
047000000801     C                   PARM      '0'           PiInt             1
047100000801     C                   PARM      '0'           PiNum             1
047200000801     C                   IF        %error
047300000801     C                   EVAL      PiInt=*off
047400030107     C                   EVAL      PiNum=*off
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
053400021017
053500050419
053600050419
053700050419
053800050419     C     calbarcode    BEGSR
053900050419     C*
054000050419     C* Inizializzo la ds d procedura x calcolo barcode standard BARTOLINI e variabili d wrk
054100090916     C                   clear                   TRUL28DSX
054200050419     C                   clear                   wBARCODE
054300050419     C*
054400050419     C* Valorizzo il buffer d input
054500090916     C                   eval      I284STD = 'E10'
054600050419     C                   eval      I284COD = wbar
054700050419     C                   call      'TRUL28R4'
054800090916     C                   parm                    TRUL28DSX
054900050419     C                   if        O284ERR = *blanks
055000050419     C                   eval      wBARCODE = O284COD
055100050419     C                   endif
055200050419     C*
055300050419     C                   ENDSR
055400000801
055500050419
055600050419
055700000801
055800990920      /TITLE Invio dei dati al punto operativo.
055900010202     C     invio         BEGSR
056000990920     C*
056100021017     C* 1° invio FIVAT
056200010201     C                   reset                   dscmz
056300021017     C                   move      vlrpoi        cmzdst
056400021017     C                   eval      cmzfld = 'FIVATWWR'
056500010201     C                   eval      cmzmbd = vlrhdl
056600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056700021017     C                   eval      cmzfla = 'FIVAT00F'
056800021017     C                   eval      cmzmba = 'FIVAT00F'
056900010201     C                   eval      cmznrr = *zeros
057000020305     C                   move      §ctrokvt      cmznrr
057100021018     C                   eval      cmzlba = vlrfl1
057200010201     C                   call(e)   'TIS711C'
057300010201     C                   parm                    dscmz
057400010201     C                   parm      *blanks       esito
057500010205     C                   if        %error
057600010205     C                             or cmzerr = '1'
057700010205     C                             or esito  = '1'
057800010205     C                   eval      wrkesito = '3'
057900010205     C                   else
058000010201     C*
058100021017     C* 2° invio FIVAB
058200010201     C                   reset                   dscmz
058300021017     C                   move      vlrpoi        cmzdst
058400010201     C                   eval      cmzfld = vlrfou
058500010201     C                   eval      cmzmbd = vlrhdl
058600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058700021017     C                   eval      cmzfla = 'FIVAB00F'
058800021017     C                   eval      cmzmba = 'FIVAB00F'
058900010201     C                   eval      cmznrr = *zeros
059000010201     C                   move      §ctrokvb      cmznrr
059100021018     C                   eval      cmzlba = vlrfl1
059200010201     C                   call(e)   'TIS711C'
059300010201     C                   parm                    dscmz
059400010201     C                   parm      *blanks       esito
059500010201     C                   if        %error
059600010201     C                             or cmzerr = '1'
059700010201     C                             or esito  = '1'
059800010201     C                   eval      wrkesito = '3'
059900010201     C                   endif
060000010205     C                   endif
060100990920     C*
060200000613     C                   ENDSR
060300000613     C***
060400070411
060500070411     C     *pssr         BEGSR
060600070411     C*
060700070411     C                   if        %open(tivin00r)
060800070411     C                   close     tivin00r
060900070411     C                   endif
061000070411     C                   if        %open(fivabwwr)
061100070411     C                   close     fivabwwr
061200070411     C                   endif
061300070411     C                   if        %open(fivatwwr)
061400070411     C                   close     fivatwwr
061500070411     C                   endif
061600070411     C*
061700070411     C* Effettuo la chiamata al CLLE preposto
061800070411     C                   call(e)   'TITVVTC'
061900070411     C                   parm                    parccm
062000070411     C                   parm                    parmbr
062100070411     C                   parm      '2'           paropz
062200070411     C*
062300070411     C                   eval      wrkesito = '2'
062400070411     C*
062500070411     C                   seton                                        LR
062600070411     C*
062700070411     C                   ENDSR     '*CANCL'
062800070411     C***
062900070411
063000990910
063100000613     C     *inzsr        BEGSR
063200990910     C*
063300990910     C     *entry        plist
063400990920     C                   parm                    tivlrds
063500990921     C                   parm      wrkesito      esito
063600000724     C                   parm                    prmlit
063700000710     C                   parm                    prmfir
063800000613     C*
063900000830     C* CALCOLA LA DATA CORRENTE
064000000830     C                   time                    wn14             14 0
064100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
064200000830     C                   z-add     wn8           g08dat
064300000830     C                   z-add     *zeros        g08inv
064400000830     C                   movel     '0'           g08err
064500000830     C                   call      'XSRDA8'
064600000830     C                   parm                    wlbda8
064700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
064800000830     C*
064900000613     C                   ENDSR
065000000613     C***
065100990908
065200050419     Otitvm4p   E            riepilogo         2
065300990915     O                                              'Upload via Internet'
065400990915     O                                           +1 'Traduzione TIVIN00R -'
065500021017     O                                           +1 'FIVABWWR/FIVATWWR'
065600010201     O                                           +1 'Report testate bolle'
065700990915     O          E            riepilogo   2
065800990915     O                       wrkdata
065900990915     O                       wrkora              +1
066000990915     O                       procname            +1
066100990915     O          E            riepilogo   2
066200990915     O                                              'Cliente..................:'
066300990915     O                       VABCCM        z     +1
066400990915     O          E            riepilogo   2
066500990920     O                                              'Riferimento Strategi.....:'
066600990920     O                       vlrhdl              +1
066700990915     O          E            riepilogo   2
066800990915     O                                              'Giusti...................:'
066900010201     O                       §CTROKVB      2   +  1
067000990915     O          E            riepilogo   2
067100990915     O                                              'Sbagliati e corretti.....:'
067200971022     O                       §CTRMO        2   +  1
067300990915     O          E            riepilogo   2
067400990915     O                                              'Sbagliati e scartati.....:'
067500971022     O                       §CTRNO        2   +  1
067600000613
067700050419     Otitvm4ps  E            testdett          2
067800000613     O                                              'Upload via Internet'
067900000613     O                                           +1 'Traduzione TIVIN00R -'
068000021017     O                                           +1 'FIVABWWR/FIVATWWR'
068100010201     O                                           +1 'Report testate bolle'
068200000616     O          E            testdett    3
068300000613     O                                           +2 'N° rec'
068400000613     O                                           +3 'Anteprima contenuto'
068500000616     O          E            rigadett    2
068600000613     O                       rrnum               +2
068700000621     O                       recko               +3
068800000616     O          E            findett     2
068900000613     O                       wrkdata
069000000613     O                       wrkora              +1
069100000613     O                       procname            +1
069200000616     O          E            findett     2
069300000613     O                                              'Cliente..................:'
069400000613     O                       VABCCM        z     +1
069500000616     O          E            findett     2
069600000613     O                                              'Riferimento Strategi.....:'
069700000613     O                       vlrhdl              +1
069800000616     O          E            findett     2
069900000613     O                                              'Giusti...................:'
070000010201     O                       §CTROKVB      2   +  1
070100000616     O          E            findett     2
070200000613     O                                              'Sbagliati e corretti.....:'
070300000613     O                       §CTRMO        2   +  1
070400000616     O          E            findett     2
070500000613     O                                              'Sbagliati e scartati.....:'
070600000613     O                       §CTRNO        2   +  1
070700000512** CMD - COMANDI CL
070800050419OVRPRTF FILE(TITVM4P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
070900050419OVRPRTF FILE(TITVM4PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
071000050419DLTOVR FILE(TITVM4P TITVM4PS) LVL(*)
071100000512
071200000512
