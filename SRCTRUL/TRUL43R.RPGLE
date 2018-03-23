000100960131     H*--------------------------------------------------------------*
000200960305     H* PGM. TRUL43R : Routine che controlla se esiste il luogo per  *
000300960305     H*                l'invio della comunicazione di giacenza       *
000400960131     H*--------------------------------------------------------------*
000500960131     H*                PARAMETRI:                                    *
000600960305     H*                          D43KSC=Codice del cliente           *
000700960305     H*                          D43TRV=Trovato luogo S/N            *
000800960305     H*                          D43RAG/IND/...=impostati se trovato *
000900960305     H*                                         luogo.               *
001000960131     H*--------------------------------------------------------------*
001100970807     FFNSPE03L  IF   E           K DISK
001200040622     FFNSP201L  IF   E           K DISK
001300171122     Fdpimd11L  IF   E           K DISK
001400171122     Fdpimd12L  IF   E           K DISK    rename(DPIMD100:DPIMD12)
001500960305     H*--------------------------------------------------------------*
001600960131     D* DS
001700960131     D*--------------------------------------------------------------*
001800960305     D TRUL43        E DS                  EXTNAME(TRUL43DS)
001900171122     D TRUL43_1      E DS                  EXTNAME(TRUL43DS1)
002000160223     D tisie10ds     E DS
002100171122     D tisie10ds1    E DS
002200960131      *--------------------------------------------------------------*
002300960131      *  Ciclo Principale
002400960131      *--------------------------------------------------------------*
002500960201     C     *ENTRY        PLIST
002600960305     C                   PARM                    TRUL43
002700171122     C                   PARM                    TRUL43_1
002800171122      *--
002900960305     C                   MOVEL     *BLANKS       D43RAG
003000960305     C                   MOVEL     *BLANKS       D43IND
003100970807     C                   MOVEL     *BLANKS       D43CAP
003200960305     C                   MOVEL     *BLANKS       D43LOC
003300960305     C                   MOVEL     *BLANKS       D43PRO
003400960305     C                   MOVEL     *BLANKS       D43NAZ
003500960305     C                   MOVEL     *BLANKS       D43PIV
003600960305     C                   MOVEL     *BLANKS       D43TEL
003700960305     C                   MOVEL     *BLANKS       D43TEX
003800960305     C                   MOVEL     *BLANKS       D43PER
003900960305     C                   MOVEL     *BLANKS       D43FAX
004000960305     C                   MOVEL     'N'           D43TRV
004100040622     C                   MOVEL     *BLANKS       D43MAIL
004200040622     C                   MOVEL     'N'           D43TRVM
004300160204     C                   SETON                                        RT
004400171122      *--
004500040622      *  Definisco chiave di accesso FNSPE
004600960305     C     KSPE          KLIST
004700970807     C                   KFLD                    KFLS
004800970807     C                   KFLD                    KCLI
004900960305     C                   KFLD                    KCOD
005000160204      *  Definisco chiave di accesso DPIMD
005100171122     C     Kimd11        KLIST
005200160204     C                   KFLD                    d43depot
005300160204     C                   KFLD                    imdtrc
005400171122      *--
005500171122     C     Kimd12        KLIST
005600171122     C                   KFLD                    CodDep7
005700171122     C                   KFLD                    imdtrc
005800171122      *--
005900040622      *  Definisco chiave di accesso FNSP2
006000040622     C     KSPE2         KLIST
006100040622     C                   KFLD                    KCLI
006200040622     C                   KFLD                    KCOD
006300040622     C                   KFLD                    KTPE
006400171122      *--
006500960305      *  Definisco variabili
006600970807     C     *LIKE         DEFINE    SPEFLS        KFLS
006700970807     C     *LIKE         DEFINE    SPECLI        KCLI
006800960305     C     *LIKE         DEFINE    SPECOD        KCOD
006900040622     C     *LIKE         DEFINE    SP2TPE        KTPE
007000171122      *--
007100040622     C                   MOVEL     'EM'          KTPE
007200171122      *--
007300171122     c                   clear                   CodDep7           7
007400171122      **
007500171122      **  se presente il secondo parametro con il codice depot a 7
007600171122     c                   if        %PARMS = 2
007700171122     c                   if        d43dep7 <> *blank and d43dep7 <>'0000000'
007800171122     c                   movel     d43dep7       CodDep7
007900171122     c                   end
008000171122     c                   end
008100171122      *--
008200160204     c* cerca la mail del depot dpd, se non la trova continua con il codice
008300160204     c* cliente passato
008400171122     c                   if        d43depot <> ' ' or
008500171122     c                              CodDep7 <> ' '
008600160204     c                   exsr      srdpd
008700160204     c                   return
008800160204     c                   end
008900171122      *--
009000041018      *  Controllo esistenza luogo 005 o luogo passato come parametro
009100970807     C                   MOVEL     'L'           KFLS
009200041018     c                   if        d43cod = *blanks
009300970807     C                   MOVEL     '005'         KCOD
009400041018     c                   else
009500041018     C                   MOVEL     d43cod        KCOD
009600041018     c                   end
009700171122      *--
009800960305     C                   Z-ADD     D43KSC        KCLI
009900970807     C     KSPE          CHAIN     FNSPE03L                           30
010000960305     C     *IN30         IFEQ      '0'
010100960305     C                   MOVEL     SPERAG        D43RAG
010200960305     C                   MOVEL     SPEIND        D43IND
010300970807     C                   MOVEL     SPECAP        D43CAP
010400960305     C                   MOVEL     SPELOC        D43LOC
010500960305     C                   MOVEL     SPEPRO        D43PRO
010600960305     C                   MOVEL     SPENAZ        D43NAZ
010700960305     C                   MOVEL     SPEPIV        D43PIV
010800960305     C                   MOVEL     SPETEL        D43TEL
010900960305     C                   MOVEL     SPETEX        D43TEX
011000960305     C                   MOVEL     SPEPER        D43PER
011100970807     C                   MOVEL     SPEFAX        D43FAX
011200960305     C                   MOVEL     'S'           D43TRV
011300171122      *--
011400040622      *se reperito il luogo cerco eventuale indirizzo mail per invio Giac.
011500040622     C     KSPE2         CHAIN     FNSP201L                           31
011600040622     C     *IN31         IFEQ      '0'
011700040622     C                   MOVEL     SP2EST        D43MAIL
011800040910     c                   if        d43mail <> *blank
011900040622     C                   MOVEL     'S'           D43TRVM
012000040910     c                   endif
012100960305     C                   END
012200040622     C                   END
012300171122      *--
012400160204     c**********************************************************************
012500160204     c     srdpd         begsr
012600160204     c**********************************************************************
012700171122      *--
012800160301     c                   eval      d43trvm = 'S'
012900160301     c                   eval      d43trv  = 'S'
013000171122      *--
013100160301     c*  con il DEPOT di origine del Parcel,  il TISIE10R restituisce
013200160301      *    l'indirizzo del DEPOT preso dal Cappario Originale DPD
013300160301     c                   clear                   tisie10ds
013400171122     c                   clear                   tisie10ds1
013500171122      *--
013600171122      *  con il depot a 4
013700171122     c                   if        d43depot <> *blank and d43depot <> '0000'
013800160301     c                   movel     d43depot      I10DEP
013900171122     c                   end
014000171122      *  con il depot a 7
014100171122     c                   if        CodDep7 <> *blank
014200171122     c                   movel     CodDep7       I10DEP7
014300171122     c                   end
014400171122      *--
014500160301     c                   call      'TISIE10R'
014600160301     c                   parm                    tisie10ds
014700171122     c                   parm                    tisie10ds1
014800171122      *--
014900160301      * e l'indirizzo del DEPOT a cui mandare la mail
015000160301     c                   if        O10CDPNAZ <> *blank  and
015100160301     c                             O10ERR    <> 'E'
015200160301     C                   eval      D43RAG  = O10CDPNAM1
015300160301     C                   eval      D43ind  = O10CDPADR1
015400160301     C                   eval      D43cap  = O10CDPPTC
015500160301     C                   eval      D43loc  = O10CDPCTY
015600160301     C                   eval      D43NAZ  = O10CDPNAZ
015700160301     c                   endif
015800171122      *--
015900160204     c                   clear                   imdtrc
016000171122     c                   clear                   trovata_mail      1
016100171122      *--
016200171122      *- se c'era il DEPOT lungo 7
016300171122     c                   if        CodDep7 <> *blank
016400171122     c     kimd12        chain     dpimd12l
016500171122     c                   if        %found(dpimd12l)
016600171122     c                   eval       trovata_mail ='S'
016700171122     c                   eval      d43mail = imdmail
016800171122     c                   end
016900171122     c                   else
017000171122      *
017100171122      *--  altrimenti tenta con il DEPOT lungo 4
017200171122     c     kimd11        chain     dpimd11l
017300171122     c                   if        %found(dpimd11l)
017400171122     c                   eval       trovata_mail ='S'
017500160204     c                   eval      d43mail = imdmail
017600171122     c                   end
017700171122     c                   endif
017800171122      *--
017900171122      *- se  non si agganciato alcun record
018000171122     c                   if         trovata_mail = *blank
018100171122     c     '9999'        chain     dpimd11l
018200171122     c                   if        %found(dpimd11l)
018300160301     c                   eval      d43mail = imdmail
018400160301     c                   else
018500160301     c                   eval      d43mail = 'cedtest@brt.it'
018600160204     c                   end
018700171122     c                   endif
018800171122      *--
018900160204     c                   endsr
019000171122     c**********************************************************************
