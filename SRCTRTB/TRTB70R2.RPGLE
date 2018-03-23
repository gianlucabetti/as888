000100141001     H  DATEDIT(*YMD)
000200141001     F*--------------------------------------------------------------*
000300141001     F*  Duplica le 3 tabelle 7R,7V,PXA                              *
000400891030     F*--------------------------------------------------------------*
000500960110     FTABEL00F  UF A E           K DISK
000600141001     FTNTBE01L  UF A E           K DISK
000601160303     d savcod          s                   like(tbecod)
000602160303     d savke1          s                   like(tbeke1)
000603160303     d savke2          s                   like(tbeke2)
000700931209     C*--------------------------------------------------------------*
000800960109     C     *ENTRY        PLIST
000900141001     C                   PARM                    Da_Codice         2
001000141001     C                   PARM                    a_Codice          2
001100141003     C                   PARM                    Da_Trasm          1
001200141001      *
001300141001     C                   SETON                                        LR
001400141001      *
001500141001      *   Duplica la 7R
001600141001     C                   Z-ADD     1             TBLKUT
001700141001     c                   movel     '7R'          TBLCOD
001800141001     c                   movel     da_CODICE     TBLKEY
001900141001     c     Ktab          chain     tabel00f
002000141001     c                   if        %Found(Tabel00F)
002100141001     c                   eval         TBLKEY = a_Codice
002200141003     c                   clear                   TBLFTR
002300141003     c                   clear                   TBLDTR
002400141003     C     Da_Trasm      IFEQ      'S'
002500141003     C                   MOVEL     '1'           TBLFTT
002600141003     C                   ELSE
002700141003     C                   MOVEL     ' '           TBLFTT
002800141003     C                   END
002900141001     c                   write     tabel
003000141001     c                   end
003100141001      *
003200141001      *   Duplica la 7V
003300141001     C                   Z-ADD     1             TBLKUT
003400141001     c                   movel     '7V'          TBLCOD
003500141001     c                   eval       TBLKEY = 'R' + Da_Codice
003600141001     c     Ktab          setll     tabel00f
003700141001     c                   read      tabel00f
003800141001     c                   dow       not %EoF(Tabel00F)
003900141001     c                   if        %subst(TBLKEY:1:3) = 'R' + Da_Codice
004000141001     c                   eval      %subst(TBLKEY:2:2) = a_Codice
004100141003     c                   clear                   TBLFTR
004200141003     c                   clear                   TBLDTR
004300141003     C     Da_Trasm      IFEQ      'S'
004400141003     C                   MOVEL     '1'           TBLFTT
004500141003     C                   ELSE
004600141003     C                   MOVEL     ' '           TBLFTT
004700141003     C                   END
004800141001     c                   write     tabel
004900141001     c                   read      tabel00f
005000141001     c                   else
005100141001     c                   LEAVE
005200141001     c                   end
005300141001     c                   endDo
005400141001      *
005500141001      *   Duplica la PXA
005600141001     c                   movel     'PXA'         TBECOD
005700141001     c                   eval       TBEKE1 =  Da_Codice
005800141001      *
005900141001     c     Ktbe          setll     tntbe01l
006000141001     c     Ktbe          reade     tntbe01l
006100141002     c                   dow       not %EoF(tntbe01l)
006101160303     c                   eval       savcod = tbecod
006102160303     c                   eval       savke1 = tbeke1
006103160303     c                   eval       savke2 = tbeke2
006200141002     c                   eval         TBEKE1 = a_Codice
006300141003     c                   clear                   TBEFTR
006400141003     c                   clear                   TBEDTR
006500141003     C     Da_Trasm      IFEQ      'S'
006600141003     C                   MOVEL     '1'           TBLFTT
006700141003     C                   ELSE
006800141003     C                   MOVEL     ' '           TBLFTT
006900141003     C                   END
007000141001     c                   write     tntbe000
007001160303     c                   eval       tbecod = savcod
007002160303     c                   eval       tbeke1 = savke1
007003160303     c                   eval       tbeke2 = savke2
007004160303     c     Ktbe3         setgt     tntbe01l
007100141001     c     Ktbe          reade     tntbe01l
007200141001     c                   endDo
007300141001      *
007400141001      *
007500141001     C                   RETURN
007600141001     C*--------------------------------------------------------------*
007700141001     c     Ktbe          klist
007800141001     c                   kfld                    TBECOD
007900141001     c                   kfld                    TBEKE1
008000141001      *
008001160303     c     Ktbe3         klist
008002160303     c                   kfld                    TBECOD
008003160303     c                   kfld                    TBEKE1
008004160303     c                   kfld                    TBEKE2
008005160303      *
008100141001     c     Ktab          klist
008200141001     c                   kfld                    TBLKUT
008300141001     c                   kfld                    TBLCOD
008400141001     c                   kfld                    TBLKEY
008500141001     C*--------------------------------------------------------------*
