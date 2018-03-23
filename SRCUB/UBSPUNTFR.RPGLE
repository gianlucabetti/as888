000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400130708     FFNBLP01L  IF   E           K DISK    usropn
000500130708     F                                     extfile(LibFileBLP01)
000600130708     FFNBLT01L  IF   E           K DISK    usropn
000700130708     F                                     extfile(LibFileBLT01)
000800120803     FFNBRV07L  IF   E           K DISK    usropn
000900120803     F                                     extfile(LibFileBRV07)
001000120803
001100120803
001200130708     D LibFileBLP01    s             21A   inz
001300130708     D LibFileBLT01    s             21A   inz
001400120803     D LibFileBRV07    s             21A   inz
001500120803     D currSysNeta     s              8A   inz('*NULL')
001600120809
001700130708     D wsputrov        s              1
001800130708     D wncl            S                   LIKE(bltnsc) inz
001900110913
002000110913
002100110913     D*------------------
002200110913     D* LINKING A DEFINIZIONI ESTERNE
002300110913     D*------------------
002400120803     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
002500120803     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
002600110913
002700110810
002800991027
002900120803     C*
003000120803     C* Reperisco il sistema AS/400 corrente
003100120803     C                   callp     UBRTVNETA_Rtv(currSysNeta)
003200120803     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
003300130708     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
003400130708     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
003500120803     C                   eval      LibFileBRV07 = 'FILTRA201/FNBRV07L'
003600120803     C                   else
003700130708     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
003800130708     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
003900130708     C                   eval      LibFileBRV07 = 'FILTRAPRD/FNBRV07L'
004000120803     C                   endif
004100120803     C*
004200120803     C* Apertura file "overraidati"
004300130708     C                   if        not %open(FNBLP01L)
004400130708     C                   open      FNBLP01L
004500120803     C                   endif
004600130708     C                   if        not %open(FNBLT01L)
004700130708     C                   open      FNBLT01L
004800120803     C                   endif
004900120803     C                   if        not %open(FNBRV07L)
005000120803     C                   open      FNBRV07L
005100120803     C                   endif
005200120803     C*
005300130708     C* Esecuzione operazioni di verifica
005400130708     C                   EXSR      rtvSpunte
005500120803     C*
005600120803     C* Chiusura file "overraidati"
005700130708     C                   if        %open(FNBLP01L)
005800130708     C                   close     FNBLP01L
005900120803     C                   endif
006000130708     C                   if        %open(FNBLT01L)
006100130708     C                   close     FNBLT01L
006200120803     C                   endif
006300120803     C                   if        %open(FNBRV07L)
006400120803     C                   close     FNBRV07L
006500120803     C                   endif
006600120803     C*
006700921023     C                   SETON                                        LR
006800111216
006900111216
007000111216
007100130708     C     rtvSpunte     BEGSR
007200120803     C*
007300130708     C* Inizializzo il contatore di spunte
007400130708     C                   z-add     *zeros        wContaSpunte      5 0
007500120803     C*
007600130708     C* Con la key sped. ricevuta chaino la testata bolla in partenza
007700130708     C     KEYblp01_C    chain     fnblp01l
007800130708     C                   if        %found(fnblp01l)
007900130708     C*
008000130708     C                   if        inNSC > *zeros
008100130708     C                   eval      outTotNCL = 1
008200130708     C                   else
008300130708     C                   eval      outTotNCL = blpNCL
008400130708     C                   endif
008500130708     C*
008600130708     C* colli non sequenziali
008700130708     C                   if        blpfns='S'
008800130708     C     KEYblt01_P    setll     fnblt01l
008900130708     C                   do        *hival
009000130708     C     KEYblt01_P    reade     fnblt01l
009100130708     C                   if        %eof(fnblt01l)
009200130708     C                   leave
009300130708     C                   endif
009400130708     C* se ricevuto filtro su specifico segnacollo
009500130708     C                   if        inNSC = *zeros OR
009600130708     C                             (inNSC > *zeros AND inNSC = bltNSC)
009700130708     C                   z-add     bltnsc        wncl
009800130708     C                   exsr      chkSpunte
009900130708     C                   if        wsputrov='S'
010000130708     C                   add       1             wContaSpunte
010100130708     C                   endif
010200130708     C                   endif
010300130708     C                   enddo
010400130708     C*
010500130708     C                   else
010600130708     C*
010700130708     C* colli sequenziali
010800130708     C                   z-add     blpncd        wncl
010900130708     C     blpnca        ifeq      0
011000130708     C                   z-add     blpncd        blpnca
011100130708     C                   endif
011200130708     C     wncl          dowle     blpnca
011300130708     C* se ricevuto filtro su specifico segnacollo
011400130708     C                   if        inNSC = *zeros OR
011500130708     C                             (inNSC > *zeros AND inNSC = wncl)
011600130708     C                   exsr      chkSpunte
011700130708     C                   if        wsputrov='S'
011800130708     C                   add       1             wContaSpunte
011900130708     C                   endif
012000130708     C                   if        wncl=9999999
012100130708     C                   leave
012200130708     C                   endif
012300130708     C                   endif
012400130708     C                   add       1             wncl
012500130708     C                   enddo
012600130708     C                   endif
012700130708     C*
012800130708     C                   eval      outTotSpunte = wContaSpunte
012900130708     C                   endif
013000111221     C*
013100111216     C                   ENDSR
013200991027
013300130708
013400130708
013500130708     C     chkSpunte     begsr
013600130708     C*
013700130708     C                   movel     *blanks       wsputrov          1
013800130708     C*
013900130708     C     KEYbrv07_P    setll     fnbrv07l
014000130708     C                   do        *hival
014100130708     C     KEYbrv07_P    reade     fnbrv07l
014200130708     C* esco dal ciclo per fine file o per spunta trovata
014300130708     C                   if        %eof(fnbrv07l) or wsputrov = 'S'
014400130708     C                   leave
014500130708     C                   endif
014600130708     C* considero solo spunte di categoria 5 e 2
014700130708     C                   if        brvnpg=5 or brvnpg=2
014800130708     C                   movel     'S'           wsputrov
014900130708     C                   endif
015000130708     C                   enddo
015100130708     C*
015200130708     C                   endsr
015300130708
015400130708
015500130708     C/TITLE Operazioni iniziali.
015600130708     C     *inzsr        BEGSR
015700991027     C*
015800130708     C     *ENTRY        PLIST
015900130708     C                   parm                    inAAS             4 0
016000130708     C                   parm                    inLNP             3 0
016100130708     C                   parm                    inNRS             2 0
016200130708     C                   parm                    inNSP             7 0
016300130708     C                   parm                    inNSC             7 0
016400130708     C                   parm                    outTotNCL         5 0
016500130708     C                   parm                    outTotSpunte      5 0
016600130708     C*
016700130708     C* Inizializzazione parametri di output
016800130708     C                   clear                   outTotNCL
016900130708     C                   clear                   outTotSpunte
017000120803     C*
017100120803     C* Definizione chiavi
017200120803     C*
017300130708     C* Chiave su FNBLP01L - Completa
017400130708     C     KEYblp01_C    KLIST
017500130708     C                   KFLD                    inAAS
017600130708     C                   KFLD                    inLNP
017700130708     C                   KFLD                    inNRS
017800130708     C                   KFLD                    inNSP
017900120803     C*
018000130708     C* Chiave su FNBLT01L - Parziale
018100130708     C     KEYblt01_P    KLIST
018200130708     C                   KFLD                    blpAAS
018300130708     C                   KFLD                    blpLNP
018400130708     C                   KFLD                    blpNRS
018500130708     C                   KFLD                    blpNSP
018600120803     C*
018700120803     C* Chiave su FNBRV07L - Parziale
018800120803     C     KEYbrv07_P    KLIST
018900130708     C                   KFLD                    bltFLS
019000130708     C                   KFLD                    bltLNA
019100130708     C                   KFLD                    bltNRS
019200130708     C                   KFLD                    wncl
019300030709     C*
019400991027     C                   ENDSR
