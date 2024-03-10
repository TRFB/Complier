#ifndef VECTOR_OPERATIONS_TPP
#define VECTOR_OPERATIONS_TPP

#include"symtable.hpp"

template<typename T>
bool areVectorsEqual(const std::vector<T>& vec1, const std::vector<T>& vec2) {
    if (vec1.size() != vec2.size()) {
        return false;
    }

    for (size_t i = 0; i < vec1.size(); ++i) {
        if (vec1[i] != vec2[i]) {
            return false;
        }
    }

    return true;
}
template bool areVectorsEqual<int>(const std::vector<int>& vec1, const std::vector<int>& vec2);


#endif
