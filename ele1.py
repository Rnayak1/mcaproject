position  = 0
def minimum_func(list1):
    length_list = len(list1)
    if(length_list == 0):
        return 
    minimum = list1[0]
    for i in range(length_list):
        if(minimum > list1[i]):
            minimum = list1[i]
    i = 0
    for i in range(length_list):
        if(list1[i] == minimum):
            index = i
    global position
    list1[index] = list1[position]
    list1[position] = minimum
    position = position+1

    return minimum_func(list1[position:])
