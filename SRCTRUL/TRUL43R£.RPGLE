000100960131     H
000200960131     H*--------------------------------------------------------------*
000300960305     H* PGM. TRUL43R : Routine che controlla se esiste il luogo per  *
000400960305     H*                l'invio della comunicazione di giacenza       *
000500960131     H*--------------------------------------------------------------*
000600960131     H*                PARAMETRI:                                    *
000700960305     H*                          D43KSC=Codice del cliente           *
000800960305     H*                          D43TRV=Trovato luogo S/N            *
000900960305     H*                          D43RAG/IND/...=impostati se trovato *
001000960305     H*                                         luogo.               *
001100960131     H*--------------------------------------------------------------*
001200970807     FFNSPE03L  IF   E           K DISK
001300040622     FFNSP201L  IF   E           K DISK
001301160204     Fdpimd01L  IF   E           K DISK
001400960305     H*--------------------------------------------------------------*
001500960131     D* DS
001600960131     D*--------------------------------------------------------------*
001700960305     D TRUL43        E DS                  EXTNAME(TRUL43DS)
001701160223     D tisie10ds     E DS
001800960131      *--------------------------------------------------------------*
001900960131      *  Ciclo Principale
002000960131      *--------------------------------------------------------------*
002100960201     C     *ENTRY        PLIST
002200960305     C                   PARM                    TRUL43
002300960305     C                   MOVEL     *BLANKS       D43RAG
002400960305     C                   MOVEL     *BLANKS       D43IND
002500970807     C                   MOVEL     *BLANKS       D43CAP
002600960305     C                   MOVEL     *BLANKS       D43LOC
002700960305     C                   MOVEL     *BLANKS       D43PRO
002800960305     C                   MOVEL     *BLANKS       D43NAZ
002900960305     C                   MOVEL     *BLANKS       D43PIV
003000960305     C                   MOVEL     *BLANKS       D43TEL
003100960305     C                   MOVEL     *BLANKS       D43TEX
003200960305     C                   MOVEL     *BLANKS       D43PER
003300960305     C                   MOVEL     *BLANKS       D43FAX
003400960305     C                   MOVEL     'N'           D43TRV
003500040622     C                   MOVEL     *BLANKS       D43MAIL
003600040622     C                   MOVEL     'N'           D43TRVM
003601160204     C                   SETON                                        RT
003700040622      *  Definisco chiave di accesso FNSPE
003800960305     C     KSPE          KLIST
003900970807     C                   KFLD                    KFLS
004000970807     C                   KFLD                    KCLI
004100960305     C                   KFLD                    KCOD
004101160204      *  Definisco chiave di accesso DPIMD
004102160204     C     Kimd          KLIST
004103160204     C                   KFLD                    d43depot
004104160204     C                   KFLD                    imdtrc
004200040622      *  Definisco chiave di accesso FNSP2
004300040622     C     KSPE2         KLIST
004400040622     C                   KFLD                    KCLI
004500040622     C                   KFLD                    KCOD
004600040622     C                   KFLD                    KTPE
004700960305      *  Definisco variabili
004800970807     C     *LIKE         DEFINE    SPEFLS        KFLS
004900970807     C     *LIKE         DEFINE    SPECLI        KCLI
005000960305     C     *LIKE         DEFINE    SPECOD        KCOD
005100040622     C     *LIKE         DEFINE    SP2TPE        KTPE
005200040622     C                   MOVEL     'EM'          KTPE
005201160204     c* cerca la mail del depot dpd, se non la trova continua con il codice
005202160204     c* cliente passato
005203160204     c                   if        d43depot <> ' '
005204160204     c                   exsr      srdpd
005206160204     c                   return
005208160204     c                   end
005300041018      *  Controllo esistenza luogo 005 o luogo passato come parametro
005400970807     C                   MOVEL     'L'           KFLS
005401041018     c                   if        d43cod = *blanks
005500970807     C                   MOVEL     '005'         KCOD
005501041018     c                   else
005502041018     C                   MOVEL     d43cod        KCOD
005503041018     c                   end
005600960305     C                   Z-ADD     D43KSC        KCLI
005700970807     C     KSPE          CHAIN     FNSPE03L                           30
005800960305     C     *IN30         IFEQ      '0'
005900960305     C                   MOVEL     SPERAG        D43RAG
006000960305     C                   MOVEL     SPEIND        D43IND
006100970807     C                   MOVEL     SPECAP        D43CAP
006200960305     C                   MOVEL     SPELOC        D43LOC
006300960305     C                   MOVEL     SPEPRO        D43PRO
006400960305     C                   MOVEL     SPENAZ        D43NAZ
006500960305     C                   MOVEL     SPEPIV        D43PIV
006600960305     C                   MOVEL     SPETEL        D43TEL
006700960305     C                   MOVEL     SPETEX        D43TEX
006800960305     C                   MOVEL     SPEPER        D43PER
006900970807     C                   MOVEL     SPEFAX        D43FAX
007000960305     C                   MOVEL     'S'           D43TRV
007100040622      *se reperito il luogo cerco eventuale indirizzo mail per invio Giac.
007200040622     C     KSPE2         CHAIN     FNSP201L                           31
007300040622     C     *IN31         IFEQ      '0'
007400040622     C                   MOVEL     SP2EST        D43MAIL
007500040910     c                   if        d43mail <> *blank
007600040622     C                   MOVEL     'S'           D43TRVM
007700040910     c                   endif
007800960305     C                   END
008000040622     C                   END
008001160204     c**********************************************************************
008100160204     c     srdpd         begsr
008101160204     c**********************************************************************
008102160301     c                   eval      d43trvm = 'S'
008103160301     c                   eval      d43trv  = 'S'
008104160301     c*  con il DEPOT di origine del Parcel,  il TISIE10R restituisce
008105160301      *    l'indirizzo del DEPOT preso dal Cappario Originale DPD
008106160301     c                   clear                   tisie10ds
008107160301     c                   movel     d43depot      I10DEP
008108160301     c                   call      'TISIE10R'
008109160301     c                   parm                    tisie10ds
008110160301      * e l'indirizzo del DEPOT a cui mandare la mail
008111160301     c                   if        O10CDPNAZ <> *blank  and
008112160301     c                             O10ERR    <> 'E'
008113160301     C                   eval      D43RAG  = O10CDPNAM1
008114160301     C                   eval      D43ind  = O10CDPADR1
008115160301     C                   eval      D43cap  = O10CDPPTC
008116160301     C                   eval      D43loc  = O10CDPCTY
008117160301     C                   eval      D43NAZ  = O10CDPNAZ
008118160301     c                   endif
008119160204     c                   clear                   imdtrc
008120160204     c     kimd          chain     dpimd01l
008121160204     c                   if        %found(dpimd01l)
008122160204     c                   eval      d43mail = imdmail
008123160301     c                   else
008124160301     c     '9999'        chain     dpimd01l
008125160301     c                   if        %found(dpimd01l)
008126160301     c                   eval      d43mail = imdmail
008127160301     c                   else
008128160301     c                   eval      d43mail = 'cedtest@brt.it'
008130160204     c                   end
008131160301     c                   end
008200160204     c                   endsr
