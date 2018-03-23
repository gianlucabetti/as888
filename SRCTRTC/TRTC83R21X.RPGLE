000100960530      ***********************************************************************
000200020219      *   ESPORTA DATI BOLLE x FEDEX       ESTERO         Export            *
000300071213      *     ?   - NUOVO TRACCIATO FEDEX "020" ex "051"  -     ?
000400960530      ***********************************************************************
000500891004     H DECEDIT('0,') DATEDIT(*DMY/)
000600000207      *---------------------------------------------------------------------*
000700970204     FFNARB01L  UF   E           K DISK    COMMIT
000800020226     FFNART01L  IF   E           K DISK
000900051017     FFIARS01L  IF   E           K DISK
001000060213     Ffiar401L  IF   E           K DISK
001100031210     Ffiar501L  IF   E           K DISK
001200960530     FTABEL00F  IF   E           K DISK
001300020227     FTNTBE01L  IF   E           K DISK
001400020219     FAzOrg01L  IF   E           K DISK
001500020222     FFifed00f  O  A E             DISK
001600000207      *---------------------------------------------------------------------*
001700000207      * Schiere
001800000207      *---------------------------------------------------------------------*
001900110512     D dvpobrt       e DS
002000020220     D Num             S              1    DIM(11)
002100000207      * Codici bolla
002200960530     D C3A             S              2    DIM(200)
002300960530     D D3A             S             89    DIM(200)
002400000207      * Codici ISO nazioni x E.D.I.
002500011217     D C15             S              3    DIM(500)
002600011217     D ISO             S              3    DIM(500)
002700020219     D DNZ             S             25    DIM(500)
002800020412     D ACN             S              1    DIM(500)
002900000207      *---------------------------------------------------------------------*
003000000207      * DS
003100000207      *---------------------------------------------------------------------*
003200010424      *  Ds per reperimento tabella PT e campo risultato
003300020219     D Dati            S           1000
003400000207      *---------------------------------------------------------------------*
003500031210     D dar5gen       e ds
003600020227     D DFEDnm        E DS
003700020227     D DFEDcd        E DS
003800020227     D TRTC831       E DS                  EXTNAME(TRTC83DS1)
003900960530     D DS3A          E DS
004000961126     D DS7A2         E DS
004100030521     D DSQT1         E DS                  INZ
004200960822     D DS15          E DS
004300950428     D KPJBA         E DS
004400960530     D WLBDA8          DS                  INZ
004500950428     D  G02DAT                 1      8  0
004600950428     D  G02INV                 9     16  0
004700950428     D  G02ERR                17     17
004800950428     D  G02TGI                18     22  0
004900970203     D*  Definisco costanti per bolle reso
005000001222     D digits          C                   CONST('0123456789')
005100020222     C***********************************************************************
005200020222     C* Campi del Messaggio impostati Fissi -> eventualmente da modificare <-
005300020222     C***********************************************************************
005400071213      *   TIPO MESSAGGIO CAMIBATO  ?
005500071213     D*** InizioMsg       C                   CONST('0,"51"')
005600071213     D InizioMsg       C                   CONST('0,"020"')
005700071213      *
005800020222     D Spedizioniere   C                   CONST('BARTOLINI SPA')
005900110512     D Rifer_Bartol    C                   CONST('BARTOLINI SPA')
006000120305     D*Firma****       C                   CONST('BARTOLINI SPA')
006100120305     D Firma           C                   CONST('BARTOLINI BRT')
006200020222     D CodFedex        C                   CONST('244738842')
006300020923     D CodInEur        C                   CONST('244738842')
006400020923     D***** CodInEur        C                   CONST('249341240')
006500030820     D***** Rifer_Bartol    C                   CONST('VOLPI GIANLUCA')
006600020222     D Nr_Rifer_Bar    C                   CONST('+39-2-393381')
006700020222     D FineMsg         C                   CONST('99,""')
006800120305     D*FirmaBRT******  C                   CONST('BARTOLINI BRT')
006900120305     D FirmaBRT        C                   CONST('BRT')
007000020222     C*---------------------------------------------------------------------*
007100960530     C* Ciclo principale                                                   -*
007200960530     C*---------------------------------------------------------------------*
007300010426     C     *ENTRY        PLIST
007400010426     C                   PARM                    KPJBA
007500020227     C                   MOVEL     KPJBU         TRTC831
007600010426     C*
007700960530     C*  Se c'è numero spedizione scrivo dettaglio
007800020227    2C     F83NSP        IFNE      0
007900010426     C*
008000010426     C*-------------------
008100960530     C*  Eseguo posizionamento su archivio bolle per passaggio param.
008200020227     C                   Z-ADD     F83AAS        KAAS
008300020227     C                   Z-ADD     F83LNP        KLNP
008400020227     C                   Z-ADD     F83NRS        KNRS
008500020227     C                   Z-ADD     F83NSP        KNSP
008600020220      * B O L L A
008700960530     C     KARB          CHAIN     FNARB01L                           31
008800020219      *
008900970124    3C     *IN31         IFEQ      *OFF
009000020219      * ------------
009100020220     C* COMPOSIZIONE record BOLLA
009200020219     C                   clear                   Dati
009300020219     C* inzio record
009400020222     c                   eval      Dati = %Trim(Dati) + InizioMsg
009500020219     C* dati bolla
009600020219     c                   exsr      bolla_dati
009700020219     C* fine  record
009800020222     c                   eval      Dati = %Trim(Dati) + FineMsg
009900020220     C* scrittura
010000020222     c                   eval      fedbol = Dati
010100020222     C                   write     Fifed000
010200970124    3C                   END
010300020219     C*
010400960530     C*  Fleggo FNARB come trasmesso
010500020227     c                   if        F83DFV >0
010600960530     C                   MOVEL     'T'           ARBFT3
010700020227     C                   Z-ADD     F83DFV        ARBDT3
010800020227     c                   end
010900020227     C*
011000020227     C                   update    fnarb000
011100970124     C                   SETON                                        RT
011200970124     C*
011300970124   X2C                   ELSE
011400020219     C*
011500970124     C                   SETON                                        LR
011600020227     C*
011700970124    2C                   END
011800960530     C*--------------------------------------------------------------*
011900020219     C* dettaglio spedizione
012000960530     C*--------------------------------------------------------------*
012100020219     C     Bolla_dati    BEGSR
012200020227     C*
012300020227     C*  Dati di riferimento Responsabile Interno Bartolini
012400020227     C                   movel     'FED'         TBECOD
012500020227     C                   eval      TBEKE1='NOME RESP.     '
012600020227     C                   movel     *blanks       TBEKE2
012700020227     c                   clear                   DFEDnm
012800020227     C     KTBE          chain     TNTBE01L
012900020227     c                   if        %found(TNTBE01L)
013000020227     c                   movel     TBEuni        DFEDnm
013100020227     c                   end
013200020219     C*
013300020219     C* Concatena tutte le informazioni da inserire nel record
013400020226     C* Rif.spedizione (1)
013500020226     c                   eval      Dati = %Trim(Dati) + '1,"'                   (1)
013600020220     c                   move      arbaas        fldaas            4
013700020220     c                   move      arblnp        fldlnp            3
013800020220     c                   move      arbnrs        fldnrs            2
013900020220     c                   move      arbnsp        fldnsp            7
014000020220     c                   eval      Dati = %Trim(dati) + fldAAS +
014100020220     c                                fldLNP + fldNRS + fldNSP + '"'
014200020306     C*
014300020306     C* Imposta il Numero della Sepdizione Bartolini
014400020306     C* Rif.Sped.    (25)
014500020306     c                   eval      Dati = %Trim(Dati) + '25,"'                  (25)
014600020306     c                   move      arbaas        fldaas            4
014700020306     c                   move      arblnp        fldlnp            3
014800020306     c                   move      arbnrs        fldnrs            2
014900020306     c                   move      arbnsp        fldnsp            7
015000020306     c                   eval      Dati = %Trim(dati) + fldAAS +
015100020306     c                                fldLNP + fldNRS + fldNSP + '"'
015200020306     C*
015300020226     C* Spedizioniere Mittente (4)
015400110512     c                   if        *in66
015500020222     c                   eval      Dati = %Trim(dati) + '4,"' +
015600110512     c                             Firma         + '"'
015700110512     c                   else
015800110512     c                   eval      Dati = %Trim(dati) + '4,"' +
015900110512     c                             FirmaBRT      + '"'
016000110512     c                   end
016100020220      *
016200020220     C* Riferimenti Spedizioniere Mittente
016300020226     C* Address Sender (Bartolini) (32) (5) (6) (7) (9) (117)
016400020219     c     ArbLNA        chain     azorg01l
016500020219     c                   if        %Found(azorg01l)
016600020227     c                   if        §FEDresp <>' '
016700020227     c                   eval      Dati = %Trim(dati) + '32,"' +                (32)
016800020227     c                                    %Trim(§FEDresp) + '"'
016900020227     c                   else
017000110512     c                   if        *in66
017100020226     c                   eval      Dati = %Trim(dati) + '32,"' +                (32)
017200110512     c                                     Firma        + '"'
017300110512     c                   else
017400110512     c                   eval      Dati = %Trim(dati) + '32,"' +                (32)
017500110512     c                                     FirmaBRT     + '"'
017600110512     c                   end
017700020227     c                   end
017800020226     c                   eval      Dati = %Trim(dati) + '5,"'  +                (5)
017900020220     c                                    %Trim(orgIND) + '"'
018000020220     c                   move      orgCPF        fldCPF            5
018100020226     c                   eval      Dati = %Trim(dati) + '6,"'  + %Trim(fldCPF) +(6)
018200020220     c                                    ' - ' + %Trim(orgLOC)
018300020220     C                   eval      Dati = %Trim(dati) + ' ITALIA"'
018400020226     c                   eval      Dati = %Trim(dati) + '7,"' + %Trim(orgLOC) + (7)
018500020220     c                                     '"'
018600020226     c                   eval      Dati = %Trim(dati) + '9,"' + %Trim(fldCPF) + (9)
018700020220     c                                     '"'
018800020226     c                   eval      Dati = %Trim(dati)+ '117,"IT"'               (117)
018900020227      *
019000020219     c                   else
019100020227      *
019200020227     c                   if        §FEDresp <>' '
019300020227     c                   eval      Dati = %Trim(dati) + '32,"' +                (32)
019400020227     c                                    %Trim(§FEDresp) + '"'
019500020227     c                   else
019600110512     c                   if        *in66
019700110512     c                   eval      Dati = %Trim(dati) + '32,"' + Firma        + (32)
019800020222     c                             '"'
019900110512     c                   else
020000110512     c                   eval      Dati = %Trim(dati) + '32,"' + FirmaBRT     + (32)
020100110512     c                             '"'
020200110512     c                   end
020300020227     c                   end
020400020226     c                   eval      Dati = %Trim(dati) + '5,"XX"'                (5)
020500020226     c                   eval      Dati = %Trim(dati) + '6,"XX'                 (6)
020600020220     C                   eval      Dati = %Trim(dati) + ' ITALIA"'
020700020226     c                   eval      Dati = %Trim(dati) + '7,"XX"'                (7)
020800020226     c                   eval      Dati = %Trim(dati) + '9,"XX"'                (9)
020900020226     c                   eval      Dati = %Trim(dati)+ '117,"IT"'               (117)
021000020219     c                   end
021100020220      *
021200020222     C* Nostro Codice x Fedex ossia FEDEX ci ha codificati con questo codice
021300020226      *  quindi ci riconosce con questo codice (10)
021400020418      *
021500020418     C* se Nazione è intraEuropa cambia l'Account Number
021600020418     C                   Z-ADD     1             YY                3 0
021700020418     C     arbNZD        LOOKUP    C15(YY)                                32
021800020418      *
021900020227     C                   movel     'FED'         TBECOD
022000020227     C                   eval      TBEKE1='COD.BARTOLINI  '
022100020227     C                   movel     *blanks       TBEKE2
022200020227     c                   clear                   DFEDcd
022300020227     C     KTBE          chain     TNTBE01L
022400020418     c                   IF        %found(TNTBE01L)
022500020227     c                   movel     TBEuni        DFEDcd
022600020418     C* se Nazione è intraEuropa cambia l'Account Number
022700020418     C                   if        ACN(YY) = *blank and *in32=*on or
022800020418     c                                        *in32=*off
022900020227     c                   eval      Dati = %Trim(dati) + '10,"' +                (10) <-cod.Bartolini
023000020227     c                                    %Trim(§FEDcodx) + '"'                 <------ x Fedex
023100020418     c                   end
023200020418      * la nazione è fra quelle di consegna in Europa
023300020418     C                   if        ACN(YY) = '1' and *in32=*on
023400020418     c                   eval      Dati = %Trim(dati) + '10,"' +                (10) <-cod.Bartolini
023500020418     c                                    %Trim(§EURcodx) + '"'                 <------ x consegna
023600020418     c                   end                                                          in europa
023700020418     c                   ELSE
023800020412     C                   if        ACN(YY) = *blank and *in32=*on or
023900020412     c                                        *in32=*off
024000020226     c                   eval      Dati = %Trim(dati) + '10,"' +                (10) <-cod.Bartolini
024100020222     c                                    %Trim(CodFedex) + '"'                 <------ x Fedex
024200020412     c                   end
024300020412      * la nazione è fra quelle di consegna in Europa
024400020412     C                   if        ACN(YY) = '1' and *in32=*on
024500020412     c                   eval      Dati = %Trim(dati) + '10,"' +                (10) <-cod.Bartolini
024600020412     c                                    %Trim(CodInEur) + '"'                 <------ x consegna
024700020412     c                   end                                                          in europa
024800020418     c                   ENDIF
024900020226     C* Phone nr.sender (183)
025000020227     c                   if        §FEDtel <>' '
025100020227     c                   eval      Dati = %Trim(dati) + '183,"' +               (183)
025200020227     c                                    %Trim(§FEDtel) + '"'
025300020227     c                   else
025400020226     c                   eval      Dati = %Trim(dati) + '183,"' + Nr_Rifer_Bar +(183) <-tel.VOLPI
025500020222     c                                     '"'
025600020227     c                   end
025700020226     C* Firma del responsabile di riferimento (1203)
025800071213      *?     Cambia il tipo di dato da 1203 a 1150
025900020227     c                   if        §FEDresp <>' '
026000071213     c******             eval      Dati = %Trim(dati) + '1203,"' +              (1203)
026100071213     c*****                               %Trim(§FEDresp) + '"'
026200110512     c                   if        *in66
026300071213     c                   eval      Dati = %Trim(dati) + '1150,"' +              (1150)
026400031021     c                                    Firma + '"'
026500110512     c                   else
026600110512     c                   eval      Dati = %Trim(dati) + '1150,"' +              (1150)
026700110512     c                                    FirmaBRT + '"'
026800110512     c                   end
026900020227     c                   else
027000110512     c*****              eval      Dati = %Trim(dati) +'1203,"' + Firma        +(1203)
027100031021     c*****                                '"'
027200110512     c                   if        *in66
027300031021     c                   eval      Dati = %Trim(dati) +'1203,"' +               (1203)
027400031021     c                                    Firma + '"'
027500110512     c                   else
027600110512     c                   eval      Dati = %Trim(dati) +'1203,"' +               (1203)
027700110512     c                                    FirmaBRT + '"'
027800110512     c                   end
027900020227     c                   end
028000020226     C* Nome Destinatario (11)
028100020220     c     '"':'-'       xlate     arbRSD        fldRSD
028200020226     c                   eval      Dati = %Trim(dati) + '11,"' +                (11)
028300020220     c                                    %Trim(fldRSD) + '"'
028400031210      * ----------+
028500020226     C*  Reperisco seconda ragione sociale destinatario (12)
028600031210     C*    attenzione x Bartolini la rag.soc. è su 2 righe x tot.70 chars.
028700060213     C*     ed il secondo pezzo è su fiar4.......
028800031210     C* Andare a sostituirlo con il vero significato del campo (12 -> Fedex)
028900031210     C*  può essere un pericolo per identificare correttamente il nominativo
029000031210     C*  del destinatario straniero.
029100031210     C* Si può effettuare un concatenamento delle informazioni.
029200020219     C                   MOVEL     'D'           KTRC
029300060213     C     KAR4          CHAIN     fiar401L                           32
029400031210     c  n32'"':'-'       xlate     ar4not        fldnot
029500031210      *
029600031210      *  ma se c'è il riferimento destinatario del FIAR5 prende anche
029700031210      *   questo e lo accoda al dato presente su AR4 cercando al meglio
029800031210      *    di dare il più possibile l'informazione.
029900031210     c                   movel     'GEN'         ktrd
030000031210     c                   clear                   DAr5Gen
030100031210     c     kar5          chain     fiar501L                           34
030200031210     c                   if        %Found(fiar501L)
030300031210     c                   Movel     Ar5Uni        DAr5Gen
030400031211     c                   if         §ar5ref <> *blank
030500031210     c                   eval      fldnot = %Trim(fldnot) +
030600031210     c                             %Trim('/') +
030700031210     c                             %Trim(§ar5ref)
030800031211     c                   end
030900031210     c                   end
031000031210      * Imposta comunque se l'ha trovato un riferimento destinatario
031100031210     c  n34
031200031210     corn32              eval      Dati = %Trim(dati) + '12,"' +                (12)
031300020220     c                                    %Trim(fldnot) + '"'
031400031210      * ----------+
031500020226     C* Indirizzo Destinatario (13) (14) (15)
031600020220     c     '"':'-'       xlate     arbind        fldind
031700020226     c                   eval      Dati = %Trim(dati) + '13,"' +                (13)
031800020220     c                                    %Trim(fldind) + '"'
031900020219     c*********          eval      Dati = %Trim(dati) + '14," "'
032000020220     c     '"':'-'       xlate     arbLOD        fldLOD
032100020226     c                   eval      Dati = %Trim(dati) + '15,"' +                (15)
032200020220     c                                    %Trim(fldLOD) + '"'
032300020219      *
032400020226     C*  Provincia o Stato se US o CA (16)
032500020226     c                   eval      Dati = %Trim(dati) + '16,"' +                (16)
032600020220     c                                    %Trim(arbPRD) + '"'
032700020226     C*  Post Code (17)
032800020226     c                   eval      Dati = %Trim(dati) + '17,"' +                (17)
032900020220     c                                    %Trim(arbCAD) + '"'
033000020226     C* Al momento non esiste l'informazione in Bartolini
033100020226     C*  Nr Telefono destinatario (18)
033200031210     c                   if        §ar5teld <> *blank
033300031210     c                   movel     §ar5teld      field15          15
033400031210     c                   eval      Dati = %Trim(dati) + '18,"' +                (18)
033500031210     c                              %Trim(field15) + '"'
033600031210     c                   else
033700020226     c                   eval      Dati = %Trim(dati) + '18,"(999) 999-9999"'   (18)
033800031210     c                   end
033900020226     C*
034000020226     C*  Transport Payor Account Nr.(20)
034100020304     c***********        eval      Dati = %Trim(dati) + '20,"999999999"'        (20)
034200020219     C*----------------
034300020220     C                   z-add     1             Y                 3 0
034400020220     C     arbCBO        LOOKUP    C3A(Y)                                 32
034500020220     C   32              MOVEL     D3A(Y)        DS3A
034600020220     C* Se franco frontiera imposto 'A' come se fosse un porto
034700020220     C* assegnato
034800020220     C                   MOVEL     §3ATB1        WPOR              1
034900020220     C     ARBFBR        IFEQ      'F'
035000020220     C                   MOVEL     'A'           WPOR
035100020220     C                   END
035200020220     C     §3ATB1        IFEQ      'FS'
035300020220     C                   MOVEL     'A'           WPOR
035400020220     C                   END
035500020220     C     §3ATB1        IFEQ      'AS'
035600020220     C                   MOVEL     'F'           WPOR
035700020220     C                   ENDIF
035800020219     C*
035900030317     C*  reperisce il volume/peso da CML o da Bollettazione
036000030317     c                   exsr      VOL_PES
036100030317     C*
036200020219     C* Determino peso tassabile per le spedizioni in assegnato
036300020219     C* (è il maggiore fra ARBPKF e il risultato delle moltiplicazione)
036400030317     C* Non più dalla fatturazione ma dalla bollettazione
036500030318     C**** WPOR          IFEQ      'A'
036600030317     C**** §7AKPM        MULT(H)   arbVLF        PESPRT           12 1
036700030317     C**** arbPKF        IFGT      PESPRT
036800030317     C****               Z-ADD     arbPKF        PESPRT
036900030318     C****               END
037000030318     C*******            ELSE
037100020219     C* Per le spedizioni in franco,invece,stampo il peso da fatturare
037200030317     C* ---> da bollettare non più da fatturare
037300030318     C*******            Z-ADD     arbPKF        PESPRT
037400030318     C*******            END
037500030318      *
037600030318      *  questa parte di specifiche erano state prese dal Manifest ma
037700030318      *   per Fedex il Peso deve essere quello reale preso o da CML o
037800030318      *   da Bollettato.
037900020219     C*
038000030318     C********           Z-ADD(H)  PESPRT        WPKF             12 1
038100030318     C                   Z-ADD(H)  wpes          WPKF             12 1
038200030318     C*
038300020219     C     WPKF          IFEQ      0
038400020219     C                   Z-ADD     1             WPKF
038500020219     C                   END
038600020220      *
038700020220     C                   clear                   iPKF             11
038800020220     c                   clear                   fld11            11
038900020220     C                   move      WPKF          dPKF              1
039000020220     c                   movel     wpkf          fld11
039100020220     C                   movea     fld11         Num
039200020220     C                   do        10            i                 3 0
039300020220     c                   if        Num(i)= '0' or Num(i)= ' '
039400020220     c                   eval      Num(i)=' '
039500020220     c                   else
039600020220     c                   leave
039700020220     c                   end
039800020220     c                   enddo
039900020227     C*
040000020220     C                   movea     Num           iPKF
040100020220     C*
040200020226     C*  Peso Totale della Spedizione (112)
040300071213      *?     Cambia il tipo di dato da 112 a 21 e
040400071213      *?     nel nuovo tracciato non deve + esserci il punto decimale
040500071213     c*******            eval      Dati = %Trim(dati) + '112,"' +               (112)
040600071213     c*******                      %Trim(iPKF) + '.' + dPKF + '"'
040700071213     c                   eval      Dati = %Trim(dati) + '21,"' +                (21)
040800071213     c                             %Trim(iPKF) + dPKF + '"'
040900020219     C*----------------
041000020226     C*  Service Code (22)
041100040430      **
041200040430      ** concetto di Buste è individuato dalla tariffazione
041300040430      **   per Buste "Fedex PAK" la tipologia è ="2"
041400040430      **   per Buste "Fedex ENVELOPE" la tipologia é = "6"
041500040430      **     concetto acquisito da mail di F.LELLO del 29/4/04
041600040430      **   verificato in filiale con Volpi utilizzo solo Fedex PAK quindi "2"
041700071213      *?     Cambia il tipo di dato da 22 a 1273
041800080124      *?      e si aggiunge il 1274 poichè l'informazione viene divisa
041900080124      *?      > 1273 come tipo di imballo
042000080124      *?      > 1277 come tipo servizio
042100080124      *?      >   Fedex ci dice quindi di impostare così:
042200080124      *?      >  -----------+----------------+-------------
042300080124      *?      >    cod.22   |    cod.1273    |   cod.1274
042400080124      *?      >  -----------+----------------+-------------
042500080124      *?      >       2     |       02       |      1
042600080124      *?      >       1     |       01       |      1
042700080124      *?      >       7     |       01       |     70
042800080124      *?      >  -----------+----------------+-------------
042900080124      *?
043000040430     c                   if        arbCTR >= 350 and arbCTR <= 399
043100071213     c******             eval      Dati = %Trim(dati) + '22,"2"'                (22)
043200080124     c                   eval      Dati = %Trim(dati) + '1273,"02"1274,"1"'     (1273)(1274)
043300040430     c                   else
043400040430      **
043500020221     c                   if        wpkf > 68 and arbNCL=1
043600071213     c******             eval      Dati = %Trim(dati) + '22,"7"'                (22)
043700080124     c                   eval      Dati = %Trim(dati) + '1273,"01"1274,"70"'    (1273)(1274)
043800020221     c                   end
043900020221     c                   if        arbNCL>1 and wpkf > 68
044000071213     c******             eval      Dati = %Trim(dati) + '22,"1"'                (22)
044100080124     c                   eval      Dati = %Trim(dati) + '1273,"01"1274,"1"'     (1273)(1274)
044200020221     c                   end
044300020221     c                   if        wpkf <= 68
044400071213     c******             eval      Dati = %Trim(dati) + '22,"1"'                (22)
044500080124     c                   eval      Dati = %Trim(dati) + '1273,"01"1274,"1"'     (1273)(1274)
044600020220     c                   end
044700040430      **
044800040430     c                   endIF
044900020221     C*----------------
045000020226     C*  Trans.Payment Code (23)
045100020226     c                   eval      Dati = %Trim(dati) + '23,"1"'                (23)
045200020219     C*----------------
045300020219     C*  Controllo se ci sono delle note di consegna
045400020226     C*  Reperisco note spedizione (25)
045500020219     C                   MOVEL     '8'           KTRC
045600060213     C     KAR4          CHAIN     fiar401L                           32
045700020220     C*  Shipment Ref.notes (1°campo note)             solo se c'è
045800020220     c  n32'"':'-'       xlate     ar4not        fldnot
045900030226      *
046000030226      **
046100030226     C*  Al momento (26/2/2003) si è capito con Zoffili e la Dorati che
046200030226     C*  non interessano a Fedex i nostri commenti nelle note.
046300030226     C*  Quindi lasciamo in questo campo il riferimento della nostra bolla
046400030226     C*  precedentemente impostato.
046500030226     c**n32*****         eval      Dati = %Trim(dati) + '25,"' +                (25)
046600030226     c**********                          %Trim(fldnot) + '"'
046700030226      **
046800020219     C*----------------
046900020220     C* Nazione Destinatario
047000020220     C                   Z-ADD     1             YY                3 0
047100020220     C     arbNZD        LOOKUP    C15(YY)                                32
047200020220     C   32ISO(YY)       COMP      *BLANKS                            3232
047300020227     C   32              MOVEL     ISO(YY)       codnaz            2
047400020220      *
047500020226     C*  Dest.Country Code (50)
047600020226     c                   eval      Dati = %Trim(dati) + '50,"' +                (50)
047700020220     c                                    %Trim(CodNaz) + '"'
047800020219     C*----------------
047900020226     C*  Currency Code (68)
048000020304     c                   if        arbVMD = 0
048100020304     c                   eval      Dati = %Trim(dati) + '68,"EUR"'              (68)
048200020304     c                   else
048300020226     c                   eval      Dati = %Trim(dati) + '68,"' +                (68)
048400020220     c                                    %Trim(arbVAD) + '"'
048500020304     c                   end
048600020219     C*----------------
048700020226     C*  Duties & Taxes Payment code (70)
048800071213      *?     Se paga il destinatario bisogna impostarlo a "2"
048900071213     c******             eval      Dati = %Trim(dati) + '70,"1"'                (70)
049000071213     c                   eval      Dati = %Trim(dati) + '70,"2"'                (70)
049100020219     C*----------------
049200020226     C*  Weight Type   (75)
049300020226     c                   eval      Dati = %Trim(dati) + '75,"KGS"'              (75)
049400020219     C*----------------
049500020226     C*  Package Description 1  (79)
049600020226     c                   eval      Dati = %Trim(dati) + '79,"' +                (79)
049700020220     c                                    %Trim(arbNAS) + '"'
049800020220     C*----------------
049900020226     C*  Origin Country of Manufacturing  (80)
050000020226     c                   eval      Dati = %Trim(dati) + '80,"IT"'               (80)
050100020220     C*----------------
050200071213      *?     Non deve essere + inviata
050300020226     C*  Special Handling/Service  (39)
050400071213     c*******            eval      Dati = %Trim(dati) + '39,"NNNNNNNNNNNNNN"'   (39)
050500020220     C*----------------
050600020226     C*  Total Packages shipped (116)
050700020220     c                   move      arbncl        fldNCL            5
050800020220     c                   clear                   fld11            11
050900020220     c                   move      fldNCL        fld11
051000020220     C                   movea     fld11         Num
051100020220     C                   do        10            i                 3 0
051200020220     c                   if        Num(i)= '0' or Num(i)= ' '
051300020220     c                   eval      Num(i)=' '
051400020220     c                   else
051500020220     c                   leave
051600020220     c                   end
051700020220     c                   enddo
051800020220     C                   movea     Num           fld11
051900020226     c                   eval      Dati = %Trim(dati) + '116,"' +               (116)
052000020220     c                                    %Trim(Fld11) + '"'
052100020220     C*----------------
052200020226     C*  Valore Merce (78)        (dichiarato)
052300071213      *?     Cambia il tipo di dato da 78 a 119 e
052400071213      *?     nel nuovo tracciato non deve + esserci il punto decimale
052500020220     c                   z-add     arbVMD        ValMer           11 2
052600020220     c                   clear                   fld11
052700020220     c                   move      ValMer        fld11
052800020220     C                   movea     fld11         Num
052900020220     C                   do        10            i                 3 0
053000020220     c                   if        Num(i)= '0' or Num(i)= ' '
053100020220     c                   eval      Num(i)=' '
053200020220     c                   else
053300020220     c                   leave
053400020220     c                   end
053500020220     c                   enddo
053600020220     C                   movea     Num           fld11
053700020304      *
053800020304     c                   if        arbVMD = 0
053900071213     c*********          eval      Dati = %Trim(dati) + '78,"001"'              (78)
054000071213     c                   eval      Dati = %Trim(dati) + '119,"001"'             (119)
054100020304     c                   else
054200071213     c*********          eval      Dati = %Trim(dati) + '78,"' +                (78)
054300071213     c                   eval      Dati = %Trim(dati) + '119,"' +               (119)
054400020220     c                                    %Trim(Fld11) + '"'
054500020304     c                   end
054600020220     C*----------------
054700020226     C*  Document Shipment flag (190)
054800021205     c                   if        arbCTR >= 300 and arbCTR <= 349
054900020226     c                   eval      Dati = %Trim(dati) + '190,"N"'               (190)
055000021205     c                   endIf
055100021205     c                   if        arbCTR >= 350 and arbCTR <= 399
055200021205      * documenti
055300021205     c                   eval      Dati = %Trim(dati) + '190,"Y"'               (190)
055400021205     c                   endIf
055500020220     C*----------------
055600020226     c                   z-add     0             art               5 0
055700020226     c                   movel     *zeros        segna1            7 0
055800020226     c                   movel     *zeros        segna2            7 0
055900020226     c                   movel     *zeros        ultsegna          7 0
056000020226     C     KARB          chain     FNART01L                           41
056100020226     c     *in41         doweq     *off
056200020226     c                   add       1             art
056300020226      * memorizzo il primo, il secondo e l'ultimo
056400020226     c     art           ifeq      1
056500020226     c                   z-add     artnsc        segna1
056600020226     c                   end
056700020226     c     art           ifeq      2
056800020226     c                   z-add     artnsc        segna2
056900020226     c                   end
057000020226     c                   z-add     artnsc        ultsegna
057100020226     C     KARB          reade     FNART01L                               41
057200020226     c                   enddo
057300020226     C*----------------
057400020226     C*  Tracking number  Master AWB il primo segnacollo (29)
057500071213      *?     Cambia il tipo di dato da 29 a 1222
057600020226     C                   move      'F'           KTRC
057700020226     C                   z-add     segna1        sgncollo
057800051017     C     KARS          CHAIN     fIARS01L
057900051017     c                   if        %found(fIARS01L)
058000020226     c                   movel     arsnot        fld12            12
058100071213     c********           eval      Dati = %Trim(dati) + '29,"' +                (29)
058200071213     c                   eval      Dati = %Trim(dati) + '1222,"' +              (1222)
058300020226     c                                    %Trim(fld12) + '"'
058400020226     c                   end
058500020226     C*----------------
058600020226      * imposta il secondo segnacollo (114)
058700020226     c                   if        segna1 <> ultsegna
058800020226     c                   if        segna2 <> *zeros
058900020226     C                   move      'F'           KTRC
059000020226     C                   z-add     segna2        sgncollo
059100051017     C     KARS          CHAIN     fIARS01L
059200051017     c                   if        %found(fIARS01L)
059300020226     c                   movel     arsnot        fld12
059400020227     c                   eval      Dati = %Trim(dati) + '114,"' +               (114)
059500020226     c                                    %Trim(fld12) + '"'
059600020226     c                   end
059700020226     C*----------------
059800020226      * imposta l'ultimo segnacollo   (115)
059900020226     C*----------------
060000020226     c                   if        ultsegna <> *zeros
060100020226     C                   move      'F'           KTRC
060200020226     C                   z-add     ultsegna      sgncollo
060300051017     C     KARS          CHAIN     fIARS01L
060400051017     c                   if        %found(fIARS01L)
060500020226     c                   movel     arsnot        fld12
060600020227     c                   eval      Dati = %Trim(dati) + '115,"' +               (115)
060700020226     c                                    %Trim(fld12) + '"'
060800020226     c                   end
060900020226     c                   endif
061000020226     c                   endif
061100020226     c                   end
061200020226      *
061300020219     c                   endsr
061400950428     C*****************************************************************
061500950428     C* OPERAZIONI INIZIALI
061600950428     C*****************************************************************
061700950428     C     *INZSR        BEGSR
061800010426     C*
061900960530     C* Definisco chiavi di accesso
062000950428     C     KARB          KLIST
062100960530     C                   KFLD                    KAAS
062200960530     C                   KFLD                    KLNP
062300960530     C                   KFLD                    KNRS
062400960530     C                   KFLD                    KNSP
062500020227      *
062600950428     C     KAR4          KLIST
062700960530     C                   KFLD                    KAAS
062800960530     C                   KFLD                    KLNP
062900960530     C                   KFLD                    KNRS
063000960530     C                   KFLD                    KNSP
063100960530     C                   KFLD                    KTRC
063200031210      *
063300031210     C     KAR5          KLIST
063400031210     C                   KFLD                    KAAS
063500031210     C                   KFLD                    KLNP
063600031210     C                   KFLD                    KNRS
063700031210     C                   KFLD                    KNSP
063800031210     C                   KFLD                    KTRD              3
063900020227      *
064000020226     C     KARS          KLIST
064100020226     C                   KFLD                    artfls
064200020226     C                   KFLD                    artlna
064300020226     C                   KFLD                    artnrs
064400020226     C                   KFLD                    sgncollo          7 0
064500020226     C                   KFLD                    KTRC
064600020227      *
064700960530     C     KTAB          KLIST
064800960530     C                   KFLD                    KKUT
064900960530     C                   KFLD                    KCOD
065000020227      *
065100961126     C     KTAB1         KLIST
065200961126     C                   KFLD                    KKUT
065300961126     C                   KFLD                    KCOD
065400961126     C                   KFLD                    KKEY
065500020227      *
065600020227     C     KTBE          KLIST
065700020227     C                   KFLD                    TBECOD
065800020227     C                   KFLD                    TBEKE1
065900020227     C                   KFLD                    TBEKE2
066000020227      *------------
066100960530     C*  Definisco variabili
066200960530     C     *LIKE         DEFINE    ARBAAS        KAAS
066300960530     C     *LIKE         DEFINE    ARBLNP        KLNP
066400960530     C     *LIKE         DEFINE    ARBNRS        KNRS
066500960530     C     *LIKE         DEFINE    ARBNSP        KNSP
066600960530     C     *LIKE         DEFINE    AR4TRC        KTRC
066700960530     C     *LIKE         DEFINE    TBLKUT        KKUT
066800960530     C     *LIKE         DEFINE    TBLCOD        KCOD
066900961126     C     *LIKE         DEFINE    TBLKEY        KKEY
067000020220     c     *like         define    arbRSD        fldRSD
067100020220     c     *like         define    ar4not        fldnot
067200020220     c     *like         define    arbind        fldind
067300020220     c     *like         define    arbLOD        fldLOD
067400030521     C     *LIKE         DEFINE    §qttpc        WTARA
067500020227      *------------
067600020220     C* Inizializzo variabili totale
067700960530     C* Imposto data del giorno
067800960530     C                   TIME                    W0140            14 0
067900960530     C                   MOVE      W0140         G02DAT            8 0
068000960530     C                   MOVEL     *BLANK        G02ERR
068100960530     C                   CALL      'XSRDA8'
068200960530     C                   PARM                    WLBDA8
068300960530     C                   MOVEL     G02INV        WOGGI             8 0
068400110512      *
068500110512     c                   move      'VPO'         tbecod
068600110512     c                   movel     'BRT'         tbeke1
068700110512     c                   clear                   tbeke2
068800110512     c     ktbe          chain     tntbe01l
068900110512     c                   if        %found(tntbe01l)
069000110512     c                   movel     tbeuni        dvpobrt
069100110512     c                   if        Woggi  <= §VPOBRTDTA
069200110512     c                   seton                                        66
069300110512     c                   endif
069400110512     c                   endif
069500120305      *
069600120305      ***cambio di marchio in BRT finale x il 15/3
069700120305     c                   setoff                                       66
069800120305     c                   if        Woggi  <  20120315
069900120305     c                   seton                                        66
070000120305     c                   endif
070100030521     C*
070200030521     C** CARICAMENTO DATI VARI TASSAZIONE
070300030521     C                   Z-ADD     1             KKUT
070400030521     C                   MOVEL     'QT'          kcod
070500030521     C                   MOVEL     *BLANKS       kKEY
070600030521     C                   MOVEL     '1'           kKEY
070700030521     C     KTAB1         CHAIN     TABEL00F                           30
070800030521     C  N30              MOVEL     TBLUNI        DSQT1
070900030521     C*
071000010424      *------------
071100960530     C* Caricamento Tabella codici bolla
071200960530     C                   Z-ADD     0             X                 3 0
071300960530     C                   MOVEL     '3A'          KCOD
071400960530     C                   Z-ADD     1             KKUT
071500960530     C     KTAB          CHAIN     TABEL00F                           30
071600960530     C     *IN30         DOWEQ     '0'
071700960530     C     TBLFLG        IFEQ      *BLANKS
071800960530     C                   ADD       1             X
071900960530     C                   MOVEL     TBLKEY        C3A(X)
072000960530     C                   MOVEL     TBLUNI        D3A(X)
072100960530     C                   END
072200960530     C     KTAB          READE     TABEL00F                               30
072300960530     C                   END
072400020227      *------------
072500960822     C* Caricamento Tabella codici nazioni
072600960822     C                   Z-ADD     0             X                 3 0
072700960822     C                   MOVEL     '15'          KCOD
072800960822     C                   Z-ADD     1             KKUT
072900960822     C     KTAB          CHAIN     TABEL00F                           30
073000960822     C     *IN30         DOWEQ     '0'
073100960822     C     TBLFLG        IFEQ      *BLANKS
073200960822     C                   ADD       1             X
073300960822     C                   MOVEL     TBLKEY        C15(X)
073400960822     C                   MOVEL     TBLUNI        DS15
073500960822     C                   MOVEL     §15COD        ISO(X)
073600020219     C                   MOVEL     §15des        DNZ(X)
073700020412     C                   MOVEL     §15ACN        ACN(X)
073800960822     C                   END
073900960822     C     KTAB          READE     TABEL00F                               30
074000960822     C                   END
074100020227      *------------
074200961126     C* Reperisco Kg per metro cubo per calcolo peso tassabile
074300961126     C                   MOVEL     '7A'          KCOD
074400961126     C                   MOVEL(P)  '2'           KKEY
074500961126     C     KTAB1         CHAIN     TABEL00F                           31
074600961126     C  N31              MOVEL     TBLUNI        DS7A2
074700961126     C   31              CLEAR                   DS7A2
074800960530     C*
074900960530     C                   ENDSR
075000020227     C*****************************************************************
075100030317     C*  Reperisce Peso/Volume o da CML o da Bollettato
075200030317     C*****************************************************************
075300030317     C     VOL_PES       BEGSR
075400030317     C*
075500030317     c     *like         define    arbvlb        wvol
075600030317     c     *like         define    arbpkb        wpes
075700030317      *
075800030317      * Prima cerca il volume fra bollettato e CML
075900050112     C     arbncR        ifeq      arbncl
076000030317      * se è considerata un'unica spedizione
076100030317     C                   z-add     ARBVLC        WVOL
076200030317     C                   else
076300030317     C*
076400030317     C     ARBVLC        ifge      ARBVLb
076500030317     C                   z-add     ARBVLC        WVOL
076600030317     C                   else
076700030317     C                   z-add     ARBVLb        WVOL
076800030317     C                   end
076900030317     C*
077000030317     C                   endif
077100030317      *
077200030521     C* PESO VDL - TARA
077300030521     C     §QTTPC        MULT      arbncp        WTARA
077400030521     C                   Z-ADD     arbPKC        wpes
077500030521     c                   sub(h)    wtara         wpes
077600030521      *
077700030317      * Poi cerca il peso fra bollettato e CML
077800050112     C     arbncP        ifeq      arbncl
077900030317      * se è considerata un'unica spedizione
078000030317     C                   ELSE
078100030521     C*  altrimenti se il bollettato è superiore passo il Bollettato.
078200030521     C     wpes          iflt      arbPKb
078300030317     C                   Z-ADD     arbPKb        wpes
078400030317     C                   end
078500030317     C*
078600030317     C                   ENDIF
078700030317     C*
078800030317     C                   ENDSR
078900030521     C*-----------------------------------------------------*
