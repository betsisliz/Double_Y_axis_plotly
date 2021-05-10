## ************************************************************************
## Create a function that aligns double y axis with clean ticks using dtick
## and range for plotly
## ************************************************************************

double_y_axis_alignment<- function(vec1, vec2, gridlines){
  
  ## Y1 Calculations
  
  y1_min = min(vec1)
  y1_max = max(vec1)
  
  if (y1_min < 0) { #the adjustment is different if there are negative values of y, like change in employment
    y1_range = y1_max - y1_min
  } else {
    y1_range = y1_max
  }
  
  y1_range = y1_range * 1000000  #mult by mil to account for ranges < 1
  y1_len = nchar(as.character(floor(y1_range))) #round and count the number of characters in the number
  
  y1_pow10_divisor = 10^(y1_len-1)
  y1_firstdigit = floor(y1_range / y1_pow10_divisor)
  y1_max_base = y1_pow10_divisor * y1_firstdigit / 1000000  # div by mil to account for ranges < 1
  #provides the maximum value if no negative values are present 
  #and the value to scale if negatives present
  
  y1_dtick = y1_max_base / gridlines #provides the step size between gridlines
  
  ## Y2 Calculations
  
  y2_min = min(vec2)
  y2_max = max(vec2)
  
  if (y2_min < 0) {
    y2_range = y2_max - y2_min
  } else {
    y2_range = y2_max
  }
  
  y2_range = y2_range * 1000000  #mult by mil to account for ranges < 1
  y2_len = nchar(as.character(floor(y2_range))) #round and count the number of characters in the number
  
  y2_pow10_divisor = 10^(y2_len-1)
  y2_firstdigit = floor(y2_range / y2_pow10_divisor)
  y2_max_base = y2_pow10_divisor * y2_firstdigit / 1000000  # div by mil to account for ranges < 1
  
  y2_dtick = y2_max_base / gridlines #dtick is the argument for tick step
  
  
  #**************************************************************************/
  # Capture the highest dtick ratio as your global dtick ratio.
  # All other axes will have their positive and negative ranges scaled to
  # make their dtick_ratios match the global ratio. When the ratios match,
  # the gridlines match!
  #**************************************************************************/
      
  y1_dtick_ratio = y1_range / y1_dtick
  y2_dtick_ratio = y2_range / y2_dtick
  
  global_dtick_ratio = max(y1_dtick_ratio, y2_dtick_ratio)
  
  
  # **************************************************************************/
  # Calculate Range Minimums
  # 
  # 1. This is done by first finding the negative ratio for all axes:
  #   1. what percentage of the range is coming from negative values
  #   2. multiply percentage by global ratio to get the percentage of the
  #      global ratio (percentage of total gridlines) that should be shown
  #      under the zero baseline.
  # 
  #      NEGATIVE RATIO == NUMBER OF GRIDLINES NEEDED FOR NEGATIVE 
  #  
  # 2. Capturing the highest negative ratio as the global negative ratio
  # 3. Then applying the negative ratio to all of your axis minimums to get
  #    their new proportionally scaled range minimums
  # **************************************************************************/
       
  negative = FALSE  # Are there any negative values present
  
  if (y1_min < 0) {
    negative = TRUE
    y1_negative_ratio = abs(y1_min / y1_range) * global_dtick_ratio
  } else {
    y1_negative_ratio = 0
  }
  
  if (y2_min < 0) {
    negative = TRUE
    y2_negative_ratio = abs(y2_min / y2_range) * global_dtick_ratio
  } else {
    y2_negative_ratio = 0
  }
  
  #Increase the ratio by 0.1 so that your range minimums are extended just
  #far enough to not cut off any part of your lowest value
  
  global_negative_ratio = max(y1_negative_ratio, y2_negative_ratio) + 0.1
  
  #If any negative value is present, you must proportionally extend the
  #range minimum of all axes
  
  if (negative) {
    y1_range_min = (global_negative_ratio) * y1_dtick * -1
    y2_range_min = (global_negative_ratio) * y2_dtick * -1
  } else {  # If no negatives, baseline is set to zero
    y1_range_min = 0
    y2_range_min = 0
  }
  
  
  # ************************************************************************
  # Calculate Range Maximums
  # 
  # 1. This is done by first finding the positive ratio for all axes:
  #     1. what percentage of the range is coming from positive values
  #     2. multiply percentage by global ratio to get the percentage of the
  #         global ratio (percentage of total gridlines) that should be shown
  #         above the zero baseline.
  # 
  #      POSITIVE RATIO == NUMBER OF GRIDLINES NEEDED FOR POSITIVE VALUES
  # 
  # 2. Capturing the highest positive ratio as the global positive ratio
  # 
  # 3. Then applying the positive ratio to all of your axis maximums to get
  #     their new proportionally scaled range maximums
  # **************************************************************************/
    
  y1_positive_ratio = abs(y1_max / y1_range) * global_dtick_ratio
  y2_positive_ratio = abs(y2_max / y2_range) * global_dtick_ratio
  
  #Increase the ratio by 0.1 so that your range maximums are extended just
  #far enough to not cut off any part of your highest value
  
  global_positive_ratio = max(y1_positive_ratio, y2_positive_ratio) + 0.1
  
  y1_range_max = (global_positive_ratio) * y1_dtick
  y2_range_max = (global_positive_ratio) * y2_dtick
  
  plotly_inputs=data.table(y1_range_max=y1_range_max,
                           y1_range_min=y1_range_min,
                           y2_range_max=y2_range_max,
                           y2_range_min=y2_range_min,
                           y1_dtick=y1_dtick,
                           y2_dtick=y2_dtick)

}