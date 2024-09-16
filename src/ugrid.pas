unit ugrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, BGRAreadTiff,
  DOM, Base64, Math, uutils, ucoordinates, ucurves;

type
  TGridMask = class
  protected
    FMask: TBGRABitmap;

    FMajorGridColor: TColor;
    FMinorGridColor: TColor;
    FBckgndColor: TColor;
    FTolerance: Integer;
    FThreshold: Double;
    FFixCurve: Boolean;
    FMaskSize: Integer;

    FIsValid: Boolean;
    FIsActive: Boolean;

    FOnChange: TNotifyEvent;
  private
    procedure SetMajorGridColor(const Value: TColor);
    procedure SetMinorGridColor(const Value: TColor);
    procedure SetBckgndColor(const Value: TColor);
    procedure SetTolerance(const Value: Integer);
    procedure SetThreshold(const Value: Double);
    procedure SetFixCurve(const Value: Boolean);
    procedure SetMaskSize(const Value: Integer);
    procedure SetIsValid(const Value: Boolean);
    procedure SetIsActive(const Value: Boolean);
  public
    constructor Create(Width, Height: Integer);
    destructor Destroy; override;

    procedure SetSize(Width, Height: Integer);

    procedure RemoveCartesianGrid(PlotImg: TBGRABitmap; Box: TPlotQuad);
    procedure RemovePolarGrid(PlotImg: TBGRABitmap; Box: TPlotQuad;
                              Pc: TCurvePoint);

    procedure RebuildCurve(PlotImg: TBGRABitmap; Box: TPlotQuad;
                           CurveColor: TColor);

    function ImportFromXML(Item: TDOMNode): Boolean;
    function ExportToXML(Doc: TXMLDocument): TDOMNode;

    property Mask: TBGRABitmap read FMask;
    property MajorGridColor: TColor read FMajorGridColor write SetMajorGridColor;
    property MinorGridColor: TColor read FMinorGridColor write SetMinorGridColor;
    property BckgndColor: TColor read FBckgndColor write SetBckgndColor;
    property Tolerance: Integer read FTolerance write SetTolerance;
    property Threshold: Double read FThreshold write SetThreshold;
    property FixCurve: Boolean read FFixCurve write SetFixCurve;
    property MaskSize: Integer read FMaskSize write SetMaskSize;
    property IsValid: Boolean read FIsValid write SetIsValid;
    property IsActive: Boolean read FIsActive write SetIsActive;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

//=============================|TGridMask|====================================//
constructor TGridMask.Create(Width, Height: Integer);
begin
  inherited Create;

  FMask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);

  FMajorGridColor := clBlack;
  FMinorGridColor := clGray;
  FBckgndColor := clWhite;
  FTolerance := 10;
  FThreshold := 0.5;

  FIsValid := False;
  FIsActive := False;
end;

destructor TGridMask.Destroy;
begin
  FMask.Free;

  inherited Destroy;
end;

procedure TGridMask.SetSize(Width, Height: Integer);
begin
  FMask.SetSize(Width, Height);
  FMask.FillTransparent;
end;

procedure TGridMask.SetMajorGridColor(const Value: TColor);
begin
  if (Value <> FMajorGridColor) then
  begin
    FMajorGridColor := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetMinorGridColor(const Value: TColor);
begin
  if (Value <> FMinorGridColor) then
  begin
    FMinorGridColor := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetBckgndColor(const Value: TColor);
begin
  if (Value <> FBckgndColor) then
  begin
    FBckgndColor := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetTolerance(const Value: Integer);
begin
  if (Value <> FTolerance) then
  begin
    FTolerance := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetThreshold(const Value: Double);
begin
  if (Value <> FThreshold) then
  begin
    FThreshold := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetFixCurve(const Value: Boolean);
begin
  if (Value <> FFixCurve) then
  begin
    FFixCurve := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetMaskSize(const Value: Integer);
begin
  if (Value <> FMaskSize) then
  begin
    FMaskSize := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetIsValid(const Value: Boolean);
begin
  if (Value <> FIsValid) then
  begin
    FIsValid := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.SetIsActive(const Value: Boolean);
begin
  if (Value <> FIsActive) then
  begin
    FIsActive := Value;

    // Notify the parent that the GridMask has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TGridMask.RemoveCartesianGrid(PlotImg: TBGRABitmap; Box: TPlotQuad);
const
  angle_step = 1;
var
  t_idx: Integer;
  i, j: Integer;
  x, y: Double;
  diag_len: Integer;

  p: PBGRAPixel;

  grid_points: Array of Array of Boolean;
  accumulator: THough2DMap;

  num_thetas: Integer;
  theta: Double;
  theta_count: Array of Integer;
  sin_theta, cos_theta: Array of Double;

  rho: Integer;

  C1, C2: LongInt;
  R1, G1, B1 : Byte;
  R2, G2, B2 : Byte;

  hori_count,
  vert_count,
  hori_index,
  vert_index: Integer;
  max_count_hori,
  max_count_vert: Integer;
begin
  try
    C1 := ColorToRGB(MajorGridColor);
    C2 := ColorToRGB(MinorGridColor);

    R1 := Red(C1);
    G1 := Green(C1);
    B1 := Blue(C1);
    R2 := Red(C2);
    G2 := Green(C2);
    B2 := Blue(C2);

    with PlotImg do
    begin
      // Diagonal length of image
      diag_len := Round(Sqrt(Width*Width + Height*Height));

      // Put the canvas values in an array for faster access
      SetLength(grid_points, Width, Height);
      for j := 0 to Height - 1 do
      begin
        p := Scanline[j];
        for i := 0 to Width - 1 do
        begin
          grid_points[i, j] := Box.Contains(TCurvePoint.Create(i, j)) and
                              (AreSimilar(p^.red, p^.green, p^.blue, R1, G1, B1, Tolerance) or
                               AreSimilar(p^.red, p^.green, p^.blue, R2, G2, B2, Tolerance));
          inc(p);
        end;
      end;
    end;

    // Cache some reusable values
    num_thetas := 1 + Round(180/angle_step);
    SetLength(sin_theta, num_thetas);
    SetLength(cos_theta, num_thetas);
    SetLength(theta_count, num_thetas);
    for t_idx := Low(sin_theta) to High(sin_theta) do
    begin
      theta := -90.0 + t_idx*angle_step;
      sin_theta[t_idx] := sin(theta*PI/180.0);
      cos_theta[t_idx] := cos(theta*PI/180.0);
      theta_count[t_idx] := 0;
    end;

    // Hough accumulator array of theta vs rho
    SetLength(accumulator, 2*diag_len, num_thetas);
    for i := Low(accumulator) to High(accumulator) do
      for j := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        accumulator[i, j] := 0;

    max_count_hori := 0;
    max_count_vert := 0;
    // Vote in the hough accumulator
    for i := Low(grid_points) to High(grid_points) do
      for j := Low(grid_points[Low(grid_points)]) to High(grid_points[Low(grid_points)]) do
        if grid_points[i, j] then
          for t_idx := Low(sin_theta) to High(sin_theta) do
          begin
            // Calculate rho, diag_len is added for a positive index
            rho := diag_len + Round(i*cos_theta[t_idx] + j*sin_theta[t_idx]);
            inc(accumulator[rho, t_idx]);

            if (abs(sin_theta[t_idx]) > abs(cos_theta[t_idx])) then
            begin
              // Horizontal line
              if (accumulator[rho, t_idx] > max_count_hori) then
                max_count_hori := accumulator[rho, t_idx];
            end
            else
            begin
              // Vertical line
              if (accumulator[rho, t_idx] > max_count_vert) then
                max_count_vert := accumulator[rho, t_idx];
            end;
          end;

    // Histogram of angles
    for i := Low(accumulator) to High(accumulator) do
      for t_idx := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        if (abs(sin_theta[t_idx]) > abs(cos_theta[t_idx])) then
        begin
          // Horizontal line
          if (accumulator[i, t_idx] > Threshold*max_count_hori) then
            inc(theta_count[t_idx]);
        end
        else
        begin
          // Vertical line
          if (accumulator[i, t_idx] > Threshold*max_count_vert) then
            inc(theta_count[t_idx]);
        end;

    // Determine angle for horizontal and vertical grid lines
    hori_count := 0;
    vert_count := 0;
    hori_index := 0;
    vert_index := 0;
    for i := Low(accumulator) to High(accumulator) do
      for t_idx := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        if (abs(sin_theta[t_idx]) > abs(cos_theta[t_idx])) then
        begin
          // Mostly horizontal line
          if (accumulator[i, t_idx] > Threshold*max_count_hori) then
          begin
            if (theta_count[t_idx] > hori_count) then
            begin
              hori_count := theta_count[t_idx];
              hori_index := t_idx;
            end;
          end;
        end
        else
        begin
          // Mostly vertical line
          if (accumulator[i, t_idx] > Threshold*max_count_vert) then
          begin
            if (theta_count[t_idx] > vert_count) then
            begin
              vert_count := theta_count[t_idx];
              vert_index := t_idx;
            end;
          end;
        end;

    //Draw all the points in the mask
    try
      Mask.FillTransparent;

      // Reset the status of the mask
      IsValid := False;
      IsActive := False;

      // Now, draw the lines
      R1 := Red(BckgndColor);
      G1 := Green(BckgndColor);
      B1 := Blue(BckgndColor);

      for i := Low(accumulator) to High(accumulator) do
      begin
        rho := i - diag_len;
        // Draw horizontal lines
        if (accumulator[i, hori_index] > Threshold*max_count_hori) then
        begin
          // Fill the line with all the points that meet the color criteria
          for j := 0 to Mask.Width - 1 do
          begin
            x := j;
            y := (rho - x*cos_theta[hori_index])/sin_theta[hori_index];

            if (y >= 0) and (Round(y) < Mask.Height) and
               grid_points[Round(x), Round(y)] then
            begin
              p := Mask.Scanline[Round(y)];
              inc(p, Round(x));
              if (p^.alpha = 0) then
              begin
                p^.red := R1;
                p^.green := G1;
                p^.blue := B1;
                p^.alpha := 255;
                // Mask.SetPixel(Round(x), Round(y), BckgndColor);
              end;
            end;
          end;
        end;

        // Draw vertical lines
        if (accumulator[i, vert_index] > Threshold*max_count_vert) then
        begin
          // Fill the line with all the points that meet the color criteria
          for j := 0 to Mask.Height - 1 do
          begin
            y := j;
            x := (rho - y*sin_theta[vert_index])/cos_theta[vert_index];

            if (x >= 0) and (Round(x) < Mask.Width) and
               grid_points[Round(x), Round(y)] then
            begin
              p := Mask.Scanline[Round(y)];
              inc(p, Round(x));
              if (p^.alpha = 0) then
              begin
                p^.red := R1;
                p^.green := G1;
                p^.blue := B1;
                p^.alpha := 255;
                // Mask.SetPixel(Round(x), Round(y), BckgndColor);
              end;
            end;
          end;
        end;
      end;
    finally
      Mask.InvalidateBitmap;

      IsValid := True;
      IsActive := True;
    end;
  finally
    // Release all the dynamic arrays
    SetLength(grid_points, 0);
    SetLength(sin_theta, 0);
    SetLength(cos_theta, 0);
    SetLength(theta_count, 0);
    SetLength(accumulator, 0);
  end;
end;

procedure TGridMask.RemovePolarGrid(PlotImg: TBGRABitmap; Box: TPlotQuad; Pc: TCurvePoint);
const
  angle_step = 0.5;
var
  t_idx: Integer;
  i, j, k: Integer;
  x, y: Double;
  diag_len: Integer;

  p: PBGRAPixel;

  grid_points: Array of Array of Boolean;
  accumulator: THough2DMap;

  num_thetas: Integer;
  theta: Double;
  sin_theta, cos_theta: Array of Double;

  rho: Integer;

  C1, C2: LongInt;
  R1, G1, B1 : Byte;
  R2, G2, B2 : Byte;

  max_count: Integer;

  accum_radius: Array of Integer;
  max_radius_count: Integer;
  radius_max_count: Integer;
begin
  try
    C1 := ColorToRGB(MajorGridColor);
    C2 := ColorToRGB(MinorGridColor);

    R1 := Red(C1);
    G1 := Green(C1);
    B1 := Blue(C1);
    R2 := Red(C2);
    G2 := Green(C2);
    B2 := Blue(C2);

    with PlotImg do
    begin
      // Diagonal length of image
      diag_len := Round(Sqrt(Width*Width + Height*Height));

      // Put the canvas values in an array for faster access
      SetLength(grid_points, Width, Height);
      for j := 0 to Height - 1 do
      begin
        p := Scanline[j];
        for i := 0 to Width - 1 do
        begin
          //grid_points[i, j] := p^.red or (p^.green shl 8) or (p^.blue shl 16);
          grid_points[i, j] := Box.Contains(TCurvePoint.Create(i, j)) and
                              (AreSimilar(p^.red, p^.green, p^.blue, R1, G1, B1, Tolerance) or
                               AreSimilar(p^.red, p^.green, p^.blue, R2, G2, B2, Tolerance));
          inc(p);
        end;
      end;
    end;

    // Cache some reusable values
    num_thetas := 1 + Round(180/angle_step);
    SetLength(sin_theta, num_thetas);
    SetLength(cos_theta, num_thetas);
    for t_idx := Low(sin_theta) to High(sin_theta) do
    begin
      theta := -90.0 + t_idx*angle_step;
      sin_theta[t_idx] := sin(theta*PI/180.0);
      cos_theta[t_idx] := cos(theta*PI/180.0);
    end;

    // Hough accumulator array of theta vs rho
    SetLength(accumulator, 2*diag_len, num_thetas);
    for i := Low(accumulator) to High(accumulator) do
      for j := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        accumulator[i, j] := 0;

    max_count := 0;
    // Vote in the hough accumulator
    for i := Low(grid_points) to High(grid_points) do
      for j := Low(grid_points[Low(grid_points)]) to High(grid_points[Low(grid_points)]) do
        if grid_points[i, j] then
          for t_idx := Low(sin_theta) to High(sin_theta) do
          begin
            // Calculate rho, diag_len is added for a positive index
            rho := diag_len + Round(i*cos_theta[t_idx] + j*sin_theta[t_idx]);
            inc(accumulator[rho, t_idx]);

            if (accumulator[rho, t_idx] > max_count) then
              max_count := accumulator[rho, t_idx];
          end;

    // Find circles (all should be concentric, and centered in the origin)
    SetLength(accum_radius, diag_len);
    max_radius_count := 0;
    // Vote in the hough accumulator (for circles)
    for i := Low(grid_points) to High(grid_points) do
      for j := Low(grid_points[Low(grid_points)]) to High(grid_points[Low(grid_points)]) do
        if grid_points[i, j] then
        begin
          // Calculate radius
          rho := Round(Sqrt((i - Pc.X)*(i - Pc.X) + (j - Pc.Y)*(j - Pc.Y)));
          inc(accum_radius[rho]);

          if (accum_radius[rho] > max_radius_count) then
          begin
            radius_max_count := rho;
            max_radius_count := accum_radius[rho];
          end;
        end;

    //Draw all the points in the mask
    try
      Mask.FillTransparent;

      // Reset the status of the mask
      IsValid := False;
      IsActive := False;

      R1 := Red(BckgndColor);
      G1 := Green(BckgndColor);
      B1 := Blue(BckgndColor);

      // Now, draw the lines
      for i := Low(accumulator) to High(accumulator) do
        for j := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        begin
          rho := i - diag_len;
          // Draw lines
          if (accumulator[i, j] >= Threshold*max_count) and
             ((Pc.X*cos_theta[j] + Pc.Y*sin_theta[j] - rho) <= 2) then
          begin
            if (abs(sin_theta[j]) > abs(cos_theta[j])) then
            begin
              // Horizontal line
              // Fill the line with all the points that meet the color criteria
              for k := 0 to Mask.Width - 1 do
              begin
                x := k;
                y := (rho - x*cos_theta[j])/sin_theta[j];

                if (y >= 0) and (Round(y) < Mask.Height) and
                   grid_points[Round(x), Round(y)] then
                begin
                  p := Mask.Scanline[Round(y)];
                  inc(p, Round(x));
                  if (p^.alpha = 0) then
                  begin
                    p^.red := R1;
                    p^.green := G1;
                    p^.blue := B1;
                    p^.alpha := 255;
                    // Mask.SetPixel(Round(x), Round(y), BckgndColor);
                  end;
                end;
              end;
            end
            else
            begin
              // Vertical line
              // Fill the line with all the points that meet the color criteria
              for k := 0 to Mask.Height - 1 do
              begin
                y := k;
                x := (rho - y*sin_theta[j])/cos_theta[j];

                if (x >= 0) and (Round(x) < Mask.Width) and
                   grid_points[Round(x), Round(y)] then
                begin
                  p := Mask.Scanline[Round(y)];
                  inc(p, Round(x));
                  if (p^.alpha = 0) then
                  begin
                    p^.red := R1;
                    p^.green := G1;
                    p^.blue := B1;
                    p^.alpha := 255;
                    // Mask.SetPixel(Round(x), Round(y), BckgndColor);
                  end;
                end;
              end;
            end;
          end;
        end;

      // Now, draw the circles
      for i := 1 to High(accum_radius) do
      begin
        // Draw circles
        if ((radius_max_count*accum_radius[i] div i) >= Threshold*max_radius_count) then
          for j := 0 to Round(2*PI*i) do
          begin
            x := Pc.X + i*cos(j/i);
            y := Pc.Y + i*sin(j/i);;

            if (x >= 0) and (Round(x) < Mask.Width) and
               (y >= 0) and (Round(y) < Mask.Height) and
               grid_points[Round(x), Round(y)] then
            begin
              p := Mask.Scanline[Round(y)];
              inc(p, Round(x));
              if (p^.alpha = 0) then
              begin
                p^.red := R1;
                p^.green := G1;
                p^.blue := B1;
                p^.alpha := 255;
                // Mask.SetPixel(Round(x), Round(y), BckgndColor);
              end;
            end;
          end;
      end;
    finally
      Mask.InvalidateBitmap;

      IsValid := True;
      IsActive := True;
    end;
  finally
    // Release all the dynamic arrays
    SetLength(grid_points, 0);
    SetLength(sin_theta, 0);
    SetLength(cos_theta, 0);
    SetLength(accumulator, 0);
    SetLength(accum_radius, 0);
  end;
end;

procedure TGridMask.RebuildCurve(PlotImg: TBGRABitmap; Box: TPlotQuad;
                                 CurveColor: TColor);
var
  i, j, k, l: Integer;
  p: PBGRAPixel;

  curve_points: Array of Array of Boolean;

  C1: LongInt;
  R1, G1, B1 : Byte;

  top_pixels: TIsland;
  bottom_pixels: TIsland;
  left_pixels: TIsland;
  right_pixels: TIsland;

  function is_masked(x, y: Integer): Boolean;
  begin
    p := Mask.Scanline[y];
    inc(p, x);
    Result := (p^.alpha > 0);
  end;

begin
  try
    C1 := ColorToRGB(CurveColor);

    R1 := Red(C1);
    G1 := Green(C1);
    B1 := Blue(C1);

    // Put the canvas values in an array for faster access
    SetLength(curve_points, PlotImg.Width, PlotImg.Height);
    for j := 0 to PlotImg.Height - 1 do
    begin
      p := PlotImg.Scanline[j];
      for i := 0 to PlotImg.Width - 1 do
      begin
        curve_points[i, j] := Box.Contains(TCurvePoint.Create(i, j)) and
                              AreSimilar(p^.red, p^.green, p^.blue,
                                         R1, G1, B1, Tolerance);

        inc(p);
      end;
    end;

    top_pixels := TIsland.Create;
    bottom_pixels := TIsland.Create;
    left_pixels := TIsland.Create;
    right_pixels := TIsland.Create;

    for i := 0 to PlotImg.Width - 1 do
      for j := 0 to PlotImg.Height - 1 do
      begin
        top_pixels.Clear;
        bottom_pixels.Clear;
        left_pixels.Clear;
        right_pixels.Clear;

        if curve_points[i, j] and is_masked(i, j) then
        begin
          // Find the pixels at the edges
          for k := max(1, i - MaskSize) to min(PlotImg.Width - 2, i + MaskSize) do
            for l := max(1, j - MaskSize) to min(PlotImg.Height - 2, j + MaskSize) do
            begin
              if curve_points[k, l] and (not is_masked(k, l)) then
              begin
                if is_masked(k + 1, l) then
                  left_pixels.AddPoint(k, l);
                if is_masked(k - 1, l) then
                  right_pixels.AddPoint(k, l);
                if is_masked(k, l + 1) then
                  bottom_pixels.AddPoint(k, l);
                if is_masked(k, l - 1) then
                  top_pixels.AddPoint(k, l);
                if is_masked(k + 1, l + 1) then
                begin
                  left_pixels.AddPoint(k, l);
                  bottom_pixels.AddPoint(k, l);
                end;
                if is_masked(k + 1, l - 1) then
                begin
                  left_pixels.AddPoint(k, l);
                  top_pixels.AddPoint(k, l);
                end;
                if is_masked(k - 1, l - 1) then
                begin
                  right_pixels.AddPoint(k, l);
                  top_pixels.AddPoint(k, l);
                end;
                if is_masked(k - 1, l + 1) then
                begin
                  right_pixels.AddPoint(k, l);
                  bottom_pixels.AddPoint(k, l);
                end;
              end;
            end;
          // Connect the edge pixels
          for k := 0 to top_pixels.Count - 1 do
            for l := 0 to bottom_pixels.Count - 1 do
            begin
              Mask.DrawLine(Round(top_pixels.Point[k].X),
                            Round(top_pixels.Point[k].Y),
                            Round(bottom_pixels.Point[l].X),
                            Round(bottom_pixels.Point[l].Y),
                            BGRAPixelTransparent,//BGRA(R1, G1, B1, 255),
                            True, dmSet);
            end;

          for k := 0 to left_pixels.Count - 1 do
            for l := 0 to right_pixels.Count - 1 do
            begin
              //ShowMessage(IntToStr(k) + ', ' + IntToStr(l) + ', ' + IntToStr(i) + ', ' + IntToStr(j));
              //ShowMessage(FloatToStr(left_pixels.Point[k].X) + ', ' +
              //            FloatToStr(left_pixels.Point[k].Y) + ', ' +
              //            FloatToStr(right_pixels.Point[l].X) + ', ' +
              //            FloatToStr(right_pixels.Point[l].Y));
              Mask.DrawLine(Round(left_pixels.Point[k].X),
                            Round(left_pixels.Point[k].Y),
                            Round(right_pixels.Point[l].X),
                            Round(right_pixels.Point[l].Y),
                            BGRAPixelTransparent,//BGRA(R1, G1, B1, 255),
                            True, dmSet);
            end;
        end;
      end;
  finally
    SetLength(curve_points, 0);

    top_pixels.Free;
    bottom_pixels.Free;
    left_pixels.Free;
    right_pixels.Free;
  end;
end;

function TGridMask.ImportFromXML(Item: TDOMNode): Boolean;
var
  i, w, h: Integer;
  Stream: TMemoryStream;
  Buffer: String; // common string with the jpg info
  EncBuffer: String; // it's Base64 equivalent
  Child: TDOMNode;
begin
  Result := False;
  try
    IsValid := False;
    IsActive := False;

    FixCurve := False;

    // Create the stream to save the image
    Stream := TMemoryStream.Create;

    w := 0;
    h := 0;
    with Item.Attributes do
    begin
      for i := 0 to Length - 1 do
      begin
        if (Item[i].CompareName('Width') = 0) then
          w := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Height') = 0) then
          h := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Active') = 0) then
          IsActive := StrToBool(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('MajorGridColor') = 0) then
          MajorGridColor := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('MinorGridColor') = 0) then
          MinorGridColor := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('BckgndColor') = 0) then
          BckgndColor := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Tolerance') = 0) then
          Tolerance := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Threshold') = 0) then
          Threshold := StrToFloat(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('FixCurve') = 0) then
          FixCurve := StrToBool(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('MaskSize') = 0) then
          MaskSize := StrToInt(UTF8Encode(Item[i].NodeValue));
      end;
    end;

    Child := Item.FirstChild;
    while assigned(Child) do
    begin
      // Grid data
      if (Child.CompareName('data') = 0) then
      begin
        // Extract the image data as a Base64 string
        EncBuffer := UTF8Encode(Child.FirstChild.NodeValue);
        // Convert it back to an ordinary string
        Buffer := DecodeStringBase64(EncBuffer);

        // Put the image data in the stream
        Stream.Clear;
        Stream.Write(Pointer(Buffer)^, Length(Buffer));
        Stream.Position := 0;

        // And load the stream in the GridMask
        Mask.LoadFromStream(Stream);
      end;

      Child := Child.NextSibling;
    end;

    IsValid := True;

    Result := True;
  finally
    Stream.Free;
  end;
end;

function TGridMask.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  Stream: TMemoryStream;
  Buffer: String; // common string with the jpg info
  EncBuffer: String; // it's Base64 equivalent
  DataNode,
  CDataNode: TDOMNode;
begin
  try
    // Create the stream to save the image
    Stream := TMemoryStream.Create;

    Result := Doc.CreateElement('grid');
    with TDOMElement(Result) do
    begin
      SetAttribute('Width', UTF8Decode(IntToStr(Mask.Width)));
      SetAttribute('Height', UTF8Decode(IntToStr(Mask.Height)));
      SetAttribute('Active', UTF8Decode(BoolToStr(IsActive, True)));
      SetAttribute('MajorGridColor', UTF8Decode('$' + IntToHex(MajorGridColor, 6)));
      SetAttribute('MinorGridColor', UTF8Decode('$' + IntToHex(MinorGridColor, 6)));
      SetAttribute('BckgndColor', UTF8Decode('$' + IntToHex(BckgndColor, 6)));
      SetAttribute('Tolerance', UTF8Decode(IntToStr(Tolerance)));
      SetAttribute('Threshold', UTF8Decode(FloatToStr(Threshold)));
      SetAttribute('FixCurve', UTF8Decode(BoolToStr(FixCurve, True)));
      SetAttribute('MaskSize', UTF8Decode(IntToStr(MaskSize)));
    end;

    DataNode := Doc.CreateElement('data');
    // Now encode the grid image and save it to the XML file
    Stream.Clear;
    Stream.Position := 0;

    Mask.SaveToStreamAsPng(Stream);

    // Extract Image contents as a common string
    SetString(Buffer, Stream.Memory, Stream.Size);
    // Encode the image contents in a Base64 string
    EncBuffer := EncodeStringBase64(Buffer);
    // Store the image on a CData-section node
    CDataNode := Doc.CreateCDATASection(UTF8Decode(EncBuffer));
    DataNode.AppendChild(CDataNode);

    Result.AppendChild(DataNode);
  finally
    Stream.Free;
  end;
end;

//=============================|TGridMask|====================================//

end.

