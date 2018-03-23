000100000000     H*------------------------------------------------------------------------*
000200000104     H* IMPORTAZIONE CAPPARIO DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500060427     F*------------------------------------------------------------------------*
000600060427     F* DATA BASE
000700060427     F*------------------------------------------------------------------------*
000800060427     FDPCDP01L  UF A E           K DISK
000900060427     FWFDPCDPF  IF   E           K DISK
001000171012     FDPLEG15L  uF   E           K DISK
001100171012     FDPLEG10F  O  A E             DISK    prefix(nw_) rename(dpleg000:dplegnw)
001200060915     FTABEL00F  IF   E           K DISK
001300060427     FPRTF198   O    F  198        PRINTER
001400060427     D*----------------
001500060427     D* DS PARAMETRI DI LANCIO PGM
001600060427     D*-------------------
001700060427     D PARAM           DS                  INZ
001800060427     D  PARVER                        5  0
001900000000     D*-------------------
002000000000     D* ARCHITETTURA
002100000000     D*-------------------
002200000000     D KPJBA         E DS
002300060915     D*----------------
002400060915     D* DS ESTERNE
002500060915     D*-------------------
002600060915     D DS15          E DS                  INZ
002700060915     D*------------------
002800060915     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900060915     D*------------------
003000060915     D WLBDA8          DS                  INZ
003100060915     D  G08DAT                 1      8  0
003200060915     D  G08INV                 9     16  0
003300060915     D  G08ERR                17     17
003400060915     D  G08TGI                18     22  0
003500060426     D*-------------------
003600060531     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XCHKCHAR'
003700060531     D*-------------------
003800060531     D TxtInOut        S           2048
003900060531     D ElencoChar      S            256
004000060531     D TipoElenco      S              1
004100060531     D CharSost        S              1
004200060531     D UpperCase       S              1
004300060531     D ChkNull         S              1
004400060531     D CharNull        S              1
004500060531     D EsitoBon        S              1
004600060427     D*------------------
004700060427     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
004800060427     D*------------------
004900060427     D InStringa       S          10240    VARYING                              (stringa input)
005000060427     D InSepara        S             10                                         (separatore)
005100060428     D skSplitCSV      S           4096    DIM(1000) VARYING
005200060428     D lenInSepara     S              2  0 INZ(*zeros)
005300060428     D posDa           S              5  0 INZ(*zeros)
005400060428     D posA            S              5  0 INZ(*zeros)
005500060428     D i               s              4  0 INZ(1)
005600060428     D wSize           S             10I 0 INZ(*zeros)
005700060427     D*-------------------
005800060427     D* VARIABILI DI WRK
005900060427     D*-------------------
006000060427     D wCharCSV        s              8    inz('|')
006100060427     D wAlfa           s           4096    inz varying
006200060427     D wRiga           s            198    inz
006300060915     D skNaz           S              2    DIM(1000)
006400060915     D skNazKey        S              3    DIM(1000)
006500060915     D jNaz            S              4  0 INZ
006600060601     D*-------------------
006700060601     D* COSTANTI
006800060601     D*-------------------
006900060601     D wElencoChr      C                   const('0123456789§')
007000000000     C*------------------------------------------------------------------------*
007100000000     C* MAIN LINE
007200000000     C*------------------------------------------------------------------------*
007300060915     C*
007400060915     C* Carico le tabelle occorrenti
007500060915     C                   EXSR      CARTAB
007600060427     C*
007700060427     C* Effettuo elaborazione
007800060427     C                   EXSR      EXECDP
007900000000     C*
008000000000     C                   SETON                                        LR
008100060427     C*------------------------------------------------------------------------*
008200060427     C*  EXECDP - IMPORTA L' ARCHIVIO "DEPOTS"
008300060427     C*------------------------------------------------------------------------*
008400060427     C     EXECDP        BEGSR
008500060427     C*
008600060427     C* Scorro tutto il file d wrk
008700060427     C     *START        SETLL     WFDPCDPF
008800060427     C                   READ      WFDPCDPF
008900060427     C                   DOW       not %eof(WFDPCDPF)
009000060427     C                   IF        %subst(%trim(WDCDPDTA):1:1) <> '#'
009100060428     C*
009200060428     C* Eseguo "splittamento" dei dati
009300060428     C                   EVAL      InStringa = WDCDPDTA
009400060428     C                   EVAL      InSepara  = wCharCSV
009500060428     C                   EXSR      SPLIT
009600060428     C*
009700060428     C* Se nn reperiti elementi listo in stampa
009800060428     C                   IF        i = *zeros
009900060427     C                   EVAL      wRiga = 'WFDPCDPF' + ' ' + WDCDPDTA
010000060427     C                   EXCEPT    rigaErr
010100060427     C                   ELSE
010200060427     C*
010300060427     C* Imposto il buffer del file d output
010400060427     C                   CLEAR                   DPCDP000
010500060427     C                   EVAL      CDPVER  = PARVER
010600060428     C                   EVAL      wAlfa = %trim(skSplitCSV(1))
010700060427     C                   MOVEL     wAlfa         CDPDPC
010800060428     C                   EVAL      wAlfa = %trim(skSplitCSV(2))
010900060427     C                   MOVEL     wAlfa         CDPIATA
011000060428     C                   EVAL      wAlfa = %trim(skSplitCSV(3))
011100060508     C                   EVAL      CDPGID = wAlfa
011200060428     C                   EVAL      wAlfa = %trim(skSplitCSV(4))
011300060427     C                   MOVEL     wAlfa         CDPNAM1
011400060531     C*
011500060531     C* Effettuo la chiamata al *pgm d "bonifica"
011600060531     C                   EVAL      TxtInOut   = CDPNAM1
011700060531     C                   EVAL      EsitoBon   = *blanks
011800060531     C*
011900060531     C                   CALL      'XCHKCHAR'
012000060531     C                   PARM                    TxtInOut
012100060531     C                   PARM                    ElencoChar
012200060531     C                   PARM                    TipoElenco
012300060531     C                   PARM                    CharSost
012400060531     C                   PARM                    UpperCase
012500060531     C                   PARM                    ChkNull
012600060531     C                   PARM                    CharNull
012700060531     C                   PARM                    EsitoBon
012800060531     C*
012900060531     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
013000060531     C                   IF        EsitoBon = '1'
013100060531     C                   EVAL      CDPNAM1 = TxtInOut
013200060531     C                   ENDIF
013300060531     C*
013400060428     C                   EVAL      wAlfa = %trim(skSplitCSV(5))
013500060427     C                   MOVEL     wAlfa         CDPNAM2
013600060531     C*
013700060531     C* Effettuo la chiamata al *pgm d "bonifica"
013800060531     C                   EVAL      TxtInOut   = CDPNAM2
013900060531     C                   EVAL      EsitoBon   = *blanks
014000060531     C*
014100060531     C                   CALL      'XCHKCHAR'
014200060531     C                   PARM                    TxtInOut
014300060531     C                   PARM                    ElencoChar
014400060531     C                   PARM                    TipoElenco
014500060531     C                   PARM                    CharSost
014600060531     C                   PARM                    UpperCase
014700060531     C                   PARM                    ChkNull
014800060531     C                   PARM                    CharNull
014900060531     C                   PARM                    EsitoBon
015000060531     C*
015100060531     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
015200060531     C                   IF        EsitoBon = '1'
015300060531     C                   EVAL      CDPNAM2 = TxtInOut
015400060531     C                   ENDIF
015500060531     C*
015600060428     C                   EVAL      wAlfa = %trim(skSplitCSV(6))
015700060427     C                   MOVEL     wAlfa         CDPADR1
015800060531     C*
015900060531     C* Effettuo la chiamata al *pgm d "bonifica"
016000060531     C                   EVAL      TxtInOut   = CDPADR1
016100060531     C                   EVAL      EsitoBon   = *blanks
016200060531     C*
016300060531     C                   CALL      'XCHKCHAR'
016400060531     C                   PARM                    TxtInOut
016500060531     C                   PARM                    ElencoChar
016600060531     C                   PARM                    TipoElenco
016700060531     C                   PARM                    CharSost
016800060531     C                   PARM                    UpperCase
016900060531     C                   PARM                    ChkNull
017000060531     C                   PARM                    CharNull
017100060531     C                   PARM                    EsitoBon
017200060531     C*
017300060531     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
017400060531     C                   IF        EsitoBon = '1'
017500060531     C                   EVAL      CDPADR1 = TxtInOut
017600060531     C                   ENDIF
017700060531     C*
017800060428     C                   EVAL      wAlfa = %trim(skSplitCSV(7))
017900060427     C                   MOVEL     wAlfa         CDPADR2
018000060531     C*
018100060531     C* Effettuo la chiamata al *pgm d "bonifica"
018200060531     C                   EVAL      TxtInOut   = CDPADR2
018300060531     C                   EVAL      EsitoBon   = *blanks
018400060531     C*
018500060531     C                   CALL      'XCHKCHAR'
018600060531     C                   PARM                    TxtInOut
018700060531     C                   PARM                    ElencoChar
018800060531     C                   PARM                    TipoElenco
018900060531     C                   PARM                    CharSost
019000060531     C                   PARM                    UpperCase
019100060531     C                   PARM                    ChkNull
019200060531     C                   PARM                    CharNull
019300060531     C                   PARM                    EsitoBon
019400060531     C*
019500060531     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
019600060531     C                   IF        EsitoBon = '1'
019700060531     C                   EVAL      CDPADR2 = TxtInOut
019800060531     C                   ENDIF
019900060531     C*
020000060428     C                   EVAL      wAlfa = %trim(skSplitCSV(8))
020100060427     C                   MOVEL     wAlfa         CDPPTC
020200060428     C                   EVAL      wAlfa = %trim(skSplitCSV(9))
020300060427     C                   MOVEL     wAlfa         CDPCTY
020400060531     C*
020500060531     C* Effettuo la chiamata al *pgm d "bonifica"
020600060531     C                   EVAL      TxtInOut   = CDPCTY
020700060531     C                   EVAL      EsitoBon   = *blanks
020800060531     C*
020900060531     C                   CALL      'XCHKCHAR'
021000060531     C                   PARM                    TxtInOut
021100060531     C                   PARM                    ElencoChar
021200060531     C                   PARM                    TipoElenco
021300060531     C                   PARM                    CharSost
021400060531     C                   PARM                    UpperCase
021500060531     C                   PARM                    ChkNull
021600060531     C                   PARM                    CharNull
021700060531     C                   PARM                    EsitoBon
021800060531     C*
021900060531     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
022000060531     C                   IF        EsitoBon = '1'
022100060531     C                   EVAL      CDPCTY = TxtInOut
022200060531     C                   ENDIF
022300060531     C*
022400060428     C                   EVAL      wAlfa = %trim(skSplitCSV(10))
022500060427     C                   MOVEL     wAlfa         CDPISO2
022600060601     C*
022700060601     C                   EVAL      wAlfa = %trim(skSplitCSV(11))
022800060601     C                   MOVE      wAlfa         CDPPHN
022900060601     C* Trimmo subito
023000060601     C                   EVAL      CDPPHN = %trim(CDPPHN)
023100060601     C                   IF        CDPPHN <> *blanks
023200060601     C* Da + a 00
023300060601     C                   IF        %scan('+':CDPPHN) > 0
023400060601     C                   EVAL      CDPPHN = ' ' + CDPPHN
023500060601     C                   EVAL      CDPPHN = %subst(CDPPHN:1:%scan('+':CDPPHN)-1)
023600060601     C                                      + '00' +
023700060601     C                                      %subst(CDPPHN:%scan('+':CDPPHN)+1)
023800060601     C                   ENDIF
023900060601     C* Da (0) a '   '
024000060601     C                   Z-ADD     1             wPos              3 0
024100060601     C                   DOW       wPos <= %len(CDPPHN)-3
024200060601     C                   IF        %subst(CDPPHN:wPos:3) = '(0)'
024300060601     C                   EVAL      CDPPHN = %subst(CDPPHN:1:wPos-1)+
024400060628     C                                      %subst(CDPPHN:wPos+3)
024500060601     C                   ENDIF
024600060601     C                   ADD       1             wPos
024700060601     C                   ENDDO
024800060601     C* Ri-trimmo
024900060601     C                   EVAL      CDPPHN = %trim(CDPPHN)
025000060601     C* Da - o ' ' a §
025100060601     C                   IF        %scan('-':CDPPHN) > 0
025200060601     C                   EVAL      %subst(CDPPHN:%scan('-':CDPPHN:1):1) = '§'
025300060601     C                   ELSE
025400060601     C                   EVAL      %subst(CDPPHN:%scan(' ':CDPPHN:1):1) = '§'
025500060601     C                   ENDIF
025600060601     C* Eliminazione caratteri nn numerici
025700060601     C                   Z-ADD     1             wPos              3 0
025800060601     C                   DOW       wPos <= %len(CDPPHN)
025900060601     C                   IF        %scan(%subst(CDPPHN:wPos:1):wElencoChr) = 0
026000060601     C                   IF        wPos <= %len(CDPPHN)-1
026100060601     C                   EVAL      CDPPHN = %subst(CDPPHN:1:wPos-1)+
026200060601     C                                      %subst(CDPPHN:wPos+1)
026300060601     C                   ELSE
026400060601     C                   EVAL      CDPPHN = %subst(CDPPHN:1:wPos-1)
026500060601     C                   ENDIF
026600060601     C                   ENDIF
026700060601     C                   ADD       1             wPos
026800060601     C                   ENDDO
026900060601     C* Da § a /
027000060601     C     '§':'/'       XLATE     CDPPHN        CDPPHN
027100060601     C* Trimmo il risultato finale
027200060601     C                   EVAL      CDPPHN = %trim(CDPPHN)
027300060601     C                   ENDIF
027400060601     C*
027500060428     C                   EVAL      wAlfa = %trim(skSplitCSV(12))
027600060601     C                   MOVEL     wAlfa         CDPFAX
027700060601     C* Trimmo subito
027800060601     C                   EVAL      CDPFAX = %trim(CDPFAX)
027900060601     C                   IF        CDPFAX <> *blanks
028000060601     C* Da + a 00
028100060601     C                   IF        %scan('+':CDPFAX) > 0
028200060601     C                   EVAL      CDPFAX = ' ' + CDPFAX
028300060601     C                   EVAL      CDPFAX = %subst(CDPFAX:1:%scan('+':CDPFAX)-1)
028400060601     C                                      + '00' +
028500060601     C                                      %subst(CDPFAX:%scan('+':CDPFAX)+1)
028600060601     C                   ENDIF
028700060601     C* Da (0) a ' '
028800060601     C                   Z-ADD     1             wPos              3 0
028900060601     C                   DOW       wPos <= %len(CDPFAX)-3
029000060601     C                   IF        %subst(CDPFAX:wPos:3) = '(0)'
029100060601     C                   EVAL      CDPFAX = %subst(CDPFAX:1:wPos-1)+
029200060628     C                                      %subst(CDPFAX:wPos+3)
029300060601     C                   ENDIF
029400060601     C                   ADD       1             wPos
029500060601     C                   ENDDO
029600060601     C* Ri-trimmo
029700060601     C                   EVAL      CDPFAX = %trim(CDPFAX)
029800060601     C* Da - o ' ' a §
029900060601     C                   IF        %scan('-':CDPFAX) > 0
030000060601     C                   EVAL      %subst(CDPFAX:%scan('-':CDPFAX:1):1) = '§'
030100060601     C                   ELSE
030200060601     C                   EVAL      %subst(CDPFAX:%scan(' ':CDPFAX:1):1) = '§'
030300060601     C                   ENDIF
030400060601     C                   EVAL      CDPFAX = %trim(CDPFAX)
030500060601     C* Eliminazione caratteri nn numerici
030600060601     C                   Z-ADD     1             wPos              3 0
030700060601     C                   DOW       wPos <= %len(CDPFAX)
030800060601     C                   IF        %scan(%subst(CDPFAX:wPos:1):wElencoChr) = 0
030900060601     C                   IF        wPos <= %len(CDPPHN)-1
031000060601     C                   EVAL      CDPFAX = %subst(CDPFAX:1:wPos-1)+
031100060601     C                                      %subst(CDPFAX:wPos+1)
031200060601     C                   ELSE
031300060601     C                   EVAL      CDPFAX = %subst(CDPFAX:1:wPos-1)
031400060601     C                   ENDIF
031500060601     C                   ENDIF
031600060601     C                   ADD       1             wPos
031700060601     C                   ENDDO
031800060601     C* Da § a /
031900060601     C     '§':'/'       XLATE     CDPFAX        CDPFAX
032000060601     C* Trimmo il risultato finale
032100060601     C                   EVAL      CDPFAX = %trim(CDPFAX)
032200060601     C                   ENDIF
032300060601     C*
032400060428     C                   EVAL      wAlfa = %trim(skSplitCSV(13))
032500060427     C                   MOVEL     wAlfa         CDPEML
032600060428     C                   EVAL      wAlfa = %trim(skSplitCSV(14))
032700060427     C                   MOVE      wAlfa         CDPWEB
032800060503     C                   EVAL      CDPORMA = 'N'
032900060427     C                   EVAL      CDPATB  = *blanks
033000060915     C*
033100060915     C* Importo unicamente i record riferiti a instradamenti su nazioni servite da DPD
033200171024     c                   goto      NONFARE
033300171024      * NON deve fare più NULLA ci pensa il cappario NUOVO poichè qui non
033400171024      * abbiamo la B.U. ed essendoci una vista logica UNICA con la B.U.in
033500171024      * chiave dopo il primo record, andrebbe sempre in errore.
033600171024      * QUINDI SALTIAMO
033700171024      * E LO FA IL CAPPARIO NUOVO.
033800060919     C                   IF        CDPISO2 <> *blanks
033900060915     C                   Z-ADD     1             jNaz
034000060915     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
034100060915     C                   IF        %equal
034200171012     C                   EXSR      WRILEG
034300060919     C                   ENDIF
034400060915     C                   ENDIF
034500171024     C*
034600171024     c     NONFARE       TAG
034700060427     C*
034800060427     C* Quindi scarico il buffer
034900060427     C                   WRITE     DPCDP000
035000060427     C                   ENDIF
035100060427     C                   ENDIF
035200060427     C                   READ      WFDPCDPF
035300060427     C                   ENDDO
035400060427     C*
035500060427     C                   ENDSR
035600060428     C*------------------------------------------------------------------------*
035700060428     C* SPLIT - Routine di reperimento PORTO e VARIE fatturate
035800060428     C*------------------------------------------------------------------------*
035900060428     C     SPLIT         BEGSR
036000060428     C*
036100060428     C* Innanzitutto inizializzo i parametri d OUT
036200060428     C                   RESET                   lenInSepara
036300060428     C                   RESET                   posDa
036400060428     C                   RESET                   posA
036500060428     C                   RESET                   i
036600060428     C                   RESET                   wSize
036700060428     C*
036800060428     C* Determino subito la lunghezza del "separatore"
036900060428     C                   EVAL      lenInSepara = %len(%trim(InSepara))
037000060428     C**********
037100060428     C* Effettuo lo split della stringa dati d input
037200060428     C**********
037300060428     C*
037400060428     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
037500060428     C                   DOW       posDa <= %len(%trim(InStringa))
037600060428     C*
037700060428     C* Gestisco il 1° elemento
037800060428     C                   IF        i = 1
037900060428     C*
038000060428     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemen
038100060428     C                   EVAL      posDa = 1
038200060428     C*
038300060428     C* Eseguo lo scan x trovare la fine del primo campo
038400060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
038500060428     C                                     posDa)
038600060428     C*
038700060428     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
038800060428     C* l'intero contenuto della stringa è l'unico elemento presente
038900060428     C                   IF        posA = *zeros
039000060428     C                   CLEAR                   SkSplitCSV
039100060428     C                   EVAL      SkSplitCSV(i) = %trim(InStringa)
039200060428     C                   LEAVE
039300060428     C                   ELSE
039400060428     C*
039500060428     C* A questo punto "estraggo" l'elemento corrente
039600060428     C                   IF        posA = 1
039700060428     C                   EVAL      SkSplitCSV(i) = *blanks
039800060428     C                   ELSE
039900060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
040000060428     C                                                    (posA-posDa))
040100060428     C                   ENDIF
040200060428     C                   ENDIF
040300060428     C*
040400060428     C* X gli elementi successivi al 1°
040500060428     C                   ELSE
040600060428     C*
040700060428     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
040800060428     C                   EVAL      posDa = posA + lenInSepara
040900060428     C*
041000060428     C* Verifico che nn vi sia il campo nn valorizzato
041100060428     C                   IF        %subst(InStringa:posDa:lenInSepara) =
041200060428     C                             %trim(InSepara)
041300060428     C*
041400060428     C* Se elemento nn valorizzato skippo al prossimo
041500060428     C                   EVAL      SkSplitCSV(i) = *blanks
041600060428     C                   EVAL      posA  = posA + 1
041700060428     C                   ELSE
041800060428     C*
041900060428     C* Eseguo lo scan x trovare la fine dell'elemento corrente
042000060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
042100060428     C                                     posDa+1)
042200060428     C*
042300060428     C* Gestisco l'ultimo campo
042400060428     C                   IF        posA = *zeros
042500060428     C                   EVAL      posA = %len(%trim(InStringa)) + 1
042600060428     C*
042700060428     C* A questo punto "estraggo" l'elemento corrente
042800060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
042900060428     C                                                    (posA-posDa))
043000060428     C                   LEAVE
043100060428     C                   ELSE
043200060428     C*
043300060428     C* A questo punto "estraggo" l'elemento corrente
043400060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
043500060428     C                                                    (posA-posDa))
043600060428     C                   ENDIF
043700060428     C                   ENDIF
043800060428     C                   ENDIF
043900060428     C*
044000060428     C* Incremento il contatore d campo
044100060428     C                   EVAL      i = i +1
044200060428     C                   ENDDO
044300060428     C*
044400060428     C                   ENDSR
044500060915     C*------------------------------------------------------------------------*
044600171012     C* WRILEG - CARICAMENTO DATI TABELLATI
044700060915     C*------------------------------------------------------------------------*
044800171012     C     wriLEG        BEGSR
044900061030     C*
045000061030     C                   MOVEL     'S'           wFlgWri           1
045100061030     C*
045200061030     C* Verifico se depot corrente già presente nel file d raccordo con P.d.C.
045300171012     C     cdpDPC        SETLL     DPleg15L
045400171012     C                   IF        %equal(DPleg15L)
045500060915     C*
045600060915     C* Verifico se presente o cmq presente record valido x il depot corrente
045700171012     C     KEYleg03_C    SETLL     DPleg15L
045800171012     C                   IF        %found(DPleg15L)
045900160222     C*
046000160222     C* Verifica validità record
046100171012     C     cdpDPC        reade     DPleg15L
046200171012     C                   IF        legDPC  = cdpDPC AND
046300171012     C                             legDDE <= datcor AND
046400171012     C                             datcor <= legDSC
046500160222     C*
046600060915     C* Se presente record valido => nulla da fare
046700061030     C                   MOVEL     'N'           wFlgWri
046800160222     C*
046900160222     C* Aggiorna la nazione passata da DPD sul file del legame.
047000171012     C                   EVAL      legNAR = skNazKey(jNaz)
047100171012     C                   update    DPleg000
047200160222     C*
047300160222     C                   ENDIF
047400061030     C                   ENDIF
047500061030     C                   ENDIF
047600061030     C*
047700061030     C                   IF        wFlgWri = 'S'
047800060915     C* ...altrimenti inserisco il depot corrente
047900171012     C                   CLEAR                   dplegnw
048000171012     C                   EVAL      nw_legDPC = cdpDPC
048100171012     C                   EVAL      nw_legNAR = skNazKey(jNaz)
048200171012     C                   EVAL      nw_legKSC = *zeros
048300171012     C                   EVAL      nw_legDDE = datcor
048400171012     C                   EVAL      nw_legDSC = 20391231
048500171012     C                   EVAL      nw_legDTV = datcor
048600171012     C                   EVAL      nw_legORV = oracor
048700171012     C                   EVAL      nw_legPRU = 'BATCH'
048800171012     C                   WRITE(e)  dplegnw
048900060915     C                   ENDIF
049000060915     C*
049100060915     C                   ENDSR
049200060915     C*------------------------------------------------------------------------*
049300060915     C* CARTAB - CARICAMENTO DATI TABELLATI
049400060915     C*------------------------------------------------------------------------*
049500060915     C     cartab        BEGSR
049600060915     C*
049700060915     C* Reperisco tutte le nazioni (ISO2) servite da DPD
049800060915     C                   Z-ADD     *zeros        jNaz
049900060915     C                   CLEAR                   skNaz
050000060915     C                   CLEAR                   skNazKey
050100060915     C                   CLEAR                   DS15
050200060915     C                   EVAL      tblKUT = 1
050300060915     C                   EVAL      tblCOD = '15'
050400060915     C     KEYtabel_P    SETLL     TABEL00F
050500060915     C                   IF        %found(TABEL00F)
050600060915     C     KEYtabel_P    READE     TABEL00F
050700060915     C                   DOW       not %eof(TABEL00F)
050800060915     C                   IF        tblFLG = *blanks
050900060915     C                   EVAL      DS15 = tblUNI
051000060921     C                   IF        §15LAD > *zeros OR
051100060921     C                             §15RED = 'S'
051200060915     C                   ADD       1             jNaz
051300060915     C                   MOVEL(P)  §15COD        skNaz(jNaz)
051400060915     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
051500060915     C                   ENDIF
051600060915     C                   ENDIF
051700060915     C     KEYtabel_P    READE     TABEL00F
051800060915     C                   ENDDO
051900060915     C                   ENDIF
052000060915     C*
052100060915     C                   ENDSR
052200000000     C*------------------------------------------------------------------------*
052300980403     C* *INZSR - OPERAZIONI INIZIALI
052400000000     C*------------------------------------------------------------------------*
052500980403     C     *INZSR        BEGSR
052600000000     C*
052700000000     C* RICEVIMENTO PARAMETRI
052800000000     C     *ENTRY        PLIST
052900000000     C                   PARM                    KPJBA
053000060427     C                   MOVEL     KPJBU         PARAM
053100060915     C*
053200060915     C* CALCOLA LA DATA CORRENTE
053300060915     C                   time                    wn14             14 0
053400060915     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053500060915     C                   movel     wn14          oracor            6 0          *ORA
053600060915     C                   z-add     wn8           g08dat
053700060915     C                   z-add     *zeros        g08inv
053800060915     C                   movel     '0'           g08err
053900060915     C                   call      'XSRDA8'
054000060915     C                   parm                    wlbda8
054100060915     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054200060915     C*
054300060915     C* Definizione chiavi di lettura
054400060915     C*
054500060915     C* TABEL00F - Parziale
054600060915     C     KEYtabel_P    KLIST
054700060915     C                   KFLD                    tblKUT
054800060915     C                   KFLD                    tblCOD
054900060915     C*
055000171012     C* DPLEG15L - Completa
055100171012     C     KEYleg03_C    KLIST
055200060915     C                   KFLD                    cdpDPC
055300060915     C                   KFLD                    datcor
055400000000     C*
055500000000     C                   ENDSR
055600060427     C*------------------------------------------------------------------------*
055700060427     OPRTF198   E            rigaErr     1
055800060427     O                       wRiga
