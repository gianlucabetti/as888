000100060614     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200060614     H BNDDIR('QC2LE')
000300050414     H DECEDIT('0,') DATEDIT(*YMD/)
000400060615      **?-------------------------------------------------------------------*****
000500060613      *  Da UPLOAD                                                              *
000600160413      *  TRASCODIFICA : SCAN 18 PUDO   28 o 29                                  *
000700160413      *           Spedizioni lasciate al Parcel Shop da consegnare              *
000800160413      *            consegnate Giacenti o Rientranti dal Parcel Shop             *
000900060615      **?-------------------------------------------------------------------*****
001000160413     FtnTBE01l  IF   E           K DISK
001100160413      * ?______________________________________________________________     */
001200060614     C*? Ds Decodifica dei campi
001300160413      * ?______________________________________________________________     */
001400060614     D TIDP500       E DS                  EXTNAME(TIDP500W)
001500160413     D TNTBSCSDS     E DS
001600060615      * ?______________________________________________________________     */
001700160413      *   Ricevuta la DS  con la riga di SCAN 18 della spedizione in Consegna
001800160413      *   al Parcel SHOP  in base all'evento ricevuto reimposta il record con
001900160413      *   lo SCAN Sostitutivo preso in tabella.
002000160413      *   Elimina eventuale SCAN in attesa di dare per Consegnata la spedizione.
002100060615      * ?______________________________________________________________     */
002200160413     c     *Entry        plist
002300160413     c                   parm                    TIDP500
002400160413     C*
002500160413     c                   SETON                                        LR
002600160413     C* klist
002700160413     C     KTBL1         KLIST
002800160413     C                   KFLD                    tbeCOD
002900160413     C                   KFLD                    tbeKE1
003000160413      *
003100160413     c                   clear                   lungo             3 0
003200160413     c                   clear                   piulungo          3 0
003300160413     c                   clear                   ScanSosti         2
003400160413      *
003500160413     C                   moveL(p)  'SCS'         tbeCOD
003600160413     C                   moveL(p)  '18'          tbeke1
003700060614      *
003800160413      * Cicla sugli eventi prendendo quello con chiave più grande e più precisa
003900160413     c     KTBL1         setll     tntbe01l
004000160413     C     KTBL1         reade     tntbe01l
004100160413     c                   dow       not %EoF(tntbe01l)
004200160413      *
004300160413      *  il più preciso poichè ha la chiave (combinazione) più lunga
004400160413      *   lo salva e lo prende in considerazione
004500160413     c                   eval      lungo = %len(%trim(tbeKE2))
004600160413     c                   if        lungo > piulungo
004700160413      *
004800160413     c                   if        %subst(I5INFOTXT:1:lungo) =
004900160413     c                             %subst(tbeke2:1:lungo)
005000160413     c                   eval      piulungo  = lungo
005100160413     c                   eval      tntbSCSds = tbeuni
005200160413     c                   eval      ScanSosti = SCSSOST
005300160413     c                   end
005400160413      *
005500160413     c                   end
005600160413     C     KTBL1         reade     tntbe01l
005700160413     c                   enddo
005800160413      *
005900160413      *    se c'è lo scan da sostituire
006000160413     c                   if        ScanSosti <> *Blank
006100160413     c                   movel     ScanSosti     I5SCANT
006200160413     c                   end
006300160413      *
006400160413     c                   RETURN
006500060615      * ?________________________________________________________________   */
